#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import signal
from glob import glob
import subprocess
import re
import fire
import shutil
import json
import time
import curses
import sys
import threading
import random
import atexit
import itertools

######## General Utils ########

_active_groups = set()

def cleanup_all():
    for pgid in list(_active_groups):
        try:
            os.killpg(pgid, signal.SIGKILL)
        except ProcessLookupError:
            pass
    _active_groups.clear()

def printLog(x,f):
    if f: print(x,file=f)
    else: print(x)

def timerBench(func, *args, **kwargs):

    start_time = time.perf_counter()
    result = func(*args, **kwargs)
    end_time = time.perf_counter()
    elapsed_time = end_time - start_time
    
    return {**result, "time" : elapsed_time }

def protectPath(p):
    return "\""+p+"\""

def concat(xxs):
    r = []
    for xs in xxs:
        r.extend(xs)
    return r

timeoutOpCode = 124

def runCommand(secs, f, command):
    printLog(command, f)
    stdout, stderr = "", ""
    
    process = subprocess.Popen(
        command, 
        shell=True, 
        stdout=subprocess.PIPE, 
        stderr=subprocess.PIPE, 
        text=True,
        start_new_session=True 
    )
    
    pgid = process.pid
    _active_groups.add(pgid)
    
    try:
        try:
            stdout, stderr = process.communicate(timeout=secs)
        except subprocess.TimeoutExpired:
            try: os.killpg(pgid, signal.SIGKILL)
            except ProcessLookupError: pass
            
            stdout, stderr = process.communicate() 
            stderr += f"\nTimed out after {secs} seconds"
            
            return subprocess.CompletedProcess(command, timeoutOpCode, stdout, stderr)

        except Exception as e:
            try: os.killpg(pgid, signal.SIGKILL)
            except ProcessLookupError: pass
            
            stdout, stderr = process.communicate()
            raise e

    finally:
        if stdout: printLog(stdout, f)
        if stderr: printLog(stderr, f)
        
        if pgid in _active_groups:
            _active_groups.remove(pgid)

    return subprocess.CompletedProcess(process.args, process.returncode, stdout, stderr)

def runCommandMode(config,command):
    secs = config["timeout"]
    f = config["logfile"]
    result = runCommand(secs,f,command)
    return result

######## Benchmark Data ########

def changeExt(oldfp,newExt):
    filename,oldExt = os.path.splitext(oldfp)
    newfp = filename+"."+newExt
    if os.path.exists(newfp): return newfp
    else: return oldfp

def readBenchs(jsonfile):
    with open(jsonfile) as f:
        dta = json.load(f)
        path = dta["path"]
        benchs = dta["benchs"]
    return path,benchs

######## Benchmark Utils ########

def parseOutput(tool,out):
    lines = [ l for l in out.splitlines() if l ]
    lines.reverse()
    status = None
    if tool=="AutoHyper":
        for l in lines:
            if l.startswith("SAT"): status=True; break
            elif l.startswith("UNSAT"): status=False; break
    elif tool=="HyperLasso":
        for l in lines:
            if l.startswith("TRUE"): status=True; break
            elif l.startswith("FALSE"): status=False; break
    else: raise Exception("tool unsupported: " +  tool)
    return status

def runBench(config,n,command,expected):
    result = runCommandMode(config,command)
    if result.returncode == timeoutOpCode:
        return { "symbol" : "⏱ msg", "message" : "timeout ", "color" : "warning" }
    txt = result.stdout + "\n" + result.stderr
    status = parseOutput(config["tool"],txt)
    
    if status is None:
        return { "symbol" : "! err", "message" : "see logs", "color" : "error" }
    if expected != status:
        msg = f"Expected {expected} but got {status}\n"
        if config["logfile"]: print(msg,file=config["logfile"])
        else: print(msg)
        return { "expected" : expected, "got" : status, "result" : False }
    else: return { "expected" : expected, "got" : status, "result" : True }

def benchHyperLasso(config,name,bench):
    
    def dobench(n,b):
        
        if not "slow" in b or b["slow"]==False or config["slow"]:
            
            smvs = bench["smv"]
            hp = changeExt(config["path"] + "/" + bench["hp"],"hp")
            
            ins = " ".join(concat([ ["--input=" + protectPath (config["path"] + "/" + smv)] for i,smv in enumerate(smvs) ]))
            inf = "--formula=" + protectPath (hp)
            
            debugparams = "--debug=True" if config["debug"] else ""
            params = config["params"] if config["params"] else ""
            
            opts = bench["hlOpts"] if "hlOpts" in bench else ""
            k = bench["k"]
            command = "HyperLasso -k="+str(k) + " " + ins + " " + inf + " " + debugparams + " " + params + " " + opts
                
            expected = bench["expected"] if "expected" in bench else None
            
            return runBench(config,n,command,expected)
    
    return timerBench(dobench,name,bench)

def benchAutoHyper(config,name,bench):
    
    def dobench(n,b):
        
        if not "slow" in b or b["slow"]==False or config["slow"]:
            
            smvs = bench["asmv"] if "asmv" in bench else bench["smv"]
            hp = changeExt(config["path"] + "/" + (bench["ahp"] if "ahp" in bench else bench["hp"]),"ah")
            
            ins = "--nusmv " + " ".join(concat([ [protectPath (config["path"] + "/" + smv)] for i,smv in enumerate(smvs) ]))
            inf = protectPath (hp)
            
            debugparams = "--log" if config["debug"] else ""
            params = config["params"] if config["params"] else ""
            
            opts = bench["ahWitnessOpts"] if config["witness"] and "ahWitnessOpts" in bench else bench["ahOpts"] if "ahOpts" in bench else ""
            command = "AutoHyper " + ins + " " + inf + " " + debugparams + " " + params + " " + opts
                
            expected = bench["expected"] if "expected" in bench else None
            
            return runBench(config,n,command,expected)
    
    return timerBench(dobench,name,bench)

######## Benchmark GUI ########

spinner = itertools.cycle(['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'])

class BenchmarkGUI:
    def __init__(self, stdscr, benchmarks, classes, num_iterations):
        self.stdscr = stdscr
        self.benchmarks = benchmarks
        self.classes = classes
        self.num_iterations = num_iterations
        
        curses.start_color()
        curses.use_default_colors()
        curses.init_pair(1, curses.COLOR_GREEN, -1)   # OK
        curses.init_pair(2, curses.COLOR_CYAN, -1)    # KO
        curses.init_pair(3, curses.COLOR_MAGENTA, -1) # WARNING
        curses.init_pair(4, curses.COLOR_RED, -1)      # ERROR
        curses.init_pair(5, curses.COLOR_YELLOW, -1)   # RUNNING
        curses.init_pair(6, curses.COLOR_WHITE, -1)    # DEFAULT
        
        self.colors = {
            "ok": curses.color_pair(1),
            "ko": curses.color_pair(2),
            "warning": curses.color_pair(3),
            "error": curses.color_pair(4),
            "running": curses.color_pair(5),
            "default": curses.color_pair(6)
        }
        
        self.results = [[{"message": "[ waiting...      ]", "color" : self.colors["default"] } for _ in classes] for _ in benchmarks]
        self.running = True
        self.completed = False
        self.start_time = time.perf_counter()

        self.pad_h = len(self.benchmarks) + 10
        self.pad_w = 16 + 31 + (len(self.classes) * 20) + 20
        self.pad = curses.newpad(self.pad_h, self.pad_w)

        self.top_y = 0
        self.left_x = 0
        self.sh, self.sw = 0, 0
        
        self.current_spinner = "-"

    def worker_logic(self):
        """This function runs in the background thread."""
        try:
            for i, (group, bench) in enumerate(self.benchmarks):
                for j, cls in enumerate(self.classes):
                    result = None
                    times = []
                    has_error = False
                    for it in range(self.num_iterations):
            
                        def showits(i, j, size):
                            return (str(i) + "/" + str(j)).rjust(size)
            
                        its = showits(it + 1, self.num_iterations, 7)
                        self.results[i][j] = {"message": f"[ iter {its}    ]", "color" : self.colors["running"]}
                        
                        r = self.run_benchmark(group,bench,cls) 
                        
                        if "result" in r:
                            times.append(r["time"])
                            if result is None:
                                result = r["result"]
                            elif result != r["result"]:
                                has_error = True
                                break    
                        else:
                            has_error = True
                            break
                            
                    # Compute and display final result
                    if has_error: 
                        symbol=r["symbol"]
                        message=r["message"]
                        color=self.colors[r["color"]]
                    else:
                        symbol = "✓    " if r["got"] else "✗    "
                        color = self.colors["ok"] if result else self.colors["ko"]
                    
                        avg = sum(times) / len(times)
                        if avg < 10:
                            message = f"{avg:.5f}s"
                        elif avg < 100:
                            message = f"{avg:.4f}s"
                        elif avg < 1000:
                            message = f"{avg:.3f}s"
                            
                    self.results[i][j] = {"message" : "[ "+symbol+f": {message} ]", "color" : color }
        
        except Exception as e:
            self.worker_exception = e
            self.end_time = time.perf_counter()
            self.elapsed_time = self.end_time - self.start_time
        finally:
            self.completed = True
            self.end_time = time.perf_counter()
            self.elapsed_time = self.end_time - self.start_time

    def run_benchmark(self, group, benchmark, cls):
        time.sleep(0.1) # Simulate work
        if random.random() < 0.8:
            return {"result": True, "got": True, "time": random.uniform(0.1, 5.0)}
        else:
            return {"symbol": "! err", "message": "error   ", "color": "error" }

    def draw_to_pad(self):
        """Redraws the current state of results onto the pad."""
        self.pad.erase() # Clear the pad buffer
        
        # Header
        self.pad.addstr(0, 0, f"{'Group':<20} {'Benchmark':<30}",self.colors["default"])
        for j, cls in enumerate(self.classes):
            self.pad.addstr(0, 21 + 31 + j * 20, f"{cls:^18}",self.colors["default"])
        self.pad.hline(1, 0, '-', 21 + 31 + len(self.classes) * 20,self.colors["default"])

        # Body
        for i, (group, bench) in enumerate(self.benchmarks):
            self.pad.addstr(i + 2, 0, f"{group:<20} {bench:<30}",self.colors["default"])
            for j, _ in enumerate(self.classes):
                res = self.results[i][j]
                self.pad.addstr(i + 2, 21 + 31 + j * 20, res['message'], res["color"])
        
        self.pad.hline(len(self.benchmarks)+2, 0, '-', 21 + 31 + len(self.classes) * 20,self.colors["default"])
        
        if self.completed:
            formatted_time = time.strftime('%Hh:%Mm:%Ss', time.gmtime(self.elapsed_time))
            finalMessage = f"All benchmarks completed in {formatted_time}. Press 'q' to exit."    
        else:
            finalMessage = "Benchmarks running." + self.current_spinner + ". Press 'q' to exit."    
        self.pad.addstr(len(self.benchmarks)+3, 0, finalMessage, self.colors["default"])

    def run_all(self):
        
        try:
            self.stdscr.keypad(True)
            self.stdscr.nodelay(True)
            curses.curs_set(0)
            
            self.worker_exception = None
            
            thread = threading.Thread(target=self.worker_logic, daemon=True)
            thread.start()
            
            while self.running:
                
                if self.worker_exception:
                    raise RuntimeError(f"Background Benchmark Worker crashed: {self.worker_exception}") 
                
                self.sh, self.sw = self.stdscr.getmaxyx()
                
                self.draw_to_pad()
                try:
                    self.pad.refresh(self.top_y, self.left_x, 0, 0, self.sh - 1, self.sw - 1)
                except curses.error:
                    pass
            
                key = self.stdscr.getch()
                if key == curses.KEY_UP: self.top_y = max(0, self.top_y - 1)
                elif key == curses.KEY_DOWN: self.top_y = min(self.pad_h - self.sh, self.top_y + 1)
                elif key == ord('q'):
                    self.save_screen_to_file(self.pad, "report.txt")
                    self.running = False
                
                if self.completed and key != -1: pass 
                
                self.current_spinner = next(spinner)
                time.sleep(0.1) #FPS
        
        finally:
            self.cleanup_on_quit()

    def cleanup_on_quit(self):
        for pgid in list(_active_groups):
            try: os.killpg(pgid, signal.SIGKILL)
            except: pass
        time.sleep(0.1)
    
    def save_screen_to_file(self, win, filename):
        h, w = win.getmaxyx()
        with open(filename, 'w', encoding='utf-8') as f:
            for y in range(h):
                try:
                    row_content = win.instr(y, 0).decode('utf-8').rstrip()
                    f.write(row_content + '\n')
                except curses.error:
                    continue

######## Benchmark Results ########

errorRes = { "symbol" : "! msg", "message" : "see logs", "color" : "error" }
skippedRes = { "symbol" : "ℹ msg", "message" : "skipped ", "color" : "running" }

groups = ["Paper","CMS","Isolation","CNI","SelfStabilization","Bakery","Controller","Planning"]

allBenchs = { group : readBenchs("benchs"+group+".json") for group in groups }
    
def getPath(group):
    path,bench = allBenchs[group]
    return path

def getBenchs(group):
    path,bench = allBenchs[group]
    return bench

timeouts = {
    "AutoHyper-w"
        : {"gni_any_2x2","gni_any_3x3","gni_ndet_2x2","gni_ndet_3x3","gni_max_2x2","gni_max_3x3"}
        | {"rc_ser_3x2","rc_ser_4x3","ser_rc_3x2","ser_rc_4x3"}
        | {"cni_any_2","cni_any_3","cni_any_4","cni_lte_2", "cni_lte_3", "cni_lte_4"}
        | {"selfstab_uf_7","selfstab_gf_7","selfstab_lf_7"}
        | {"robust_crit_2","robust_crit_3","robust_ncrit_2","robust_ncrit_3"}
        | {"spec1_4x0","spec1_4x2","spec2_1x1","spec2_1x2","spec2_4x4"}
        | {"plan_3x2","plan_3x3","plan_4x2","plan_4x3","plan_4x4"}
    ,
    "AutoHyper+w"
        : {"gni_any_2x2","gni_any_3x3","gni_ndet_2x2","gni_ndet_3x3","gni_max_2x2","gni_max_3x3"}
        | {"rc_ser_3x2","rc_ser_4x3","ser_rc_3x2","ser_rc_4x3"}
        | {"cni_any_2","cni_any_3","cni_any_4","cni_lte_2", "cni_lte_3", "cni_lte_4"}
        | {"selfstab_uf_7","selfstab_gf_7","selfstab_lf_5","selfstab_lf_7"}
        | {"robust_crit_2","robust_crit_3","robust_ncrit_2","robust_ncrit_3"}
        | {"spec1_4x0","spec1_4x2","spec2_1x1","spec2_1x2","spec2_4x4"}
        | {"plan_3x2","plan_3x3","plan_4x1","plan_4x2","plan_4x3","plan_4x4"}
    ,
    "HyperLasso+w"
        : set()
    }

class DoBenchmarkGUI(BenchmarkGUI):
    
    def __init__(self, stdscr, config):
            basecols = ["AutoHyper-w","AutoHyper+w"]
            modes = ["HyperLasso"]
            tabs = ["+w"]
            cols = basecols + [ mode+tab for mode in modes for tab in tabs ]
            if config["group"]:
                group = config["group"]
                benchs = [ (group,val) for val in getBenchs(group).keys() ]
            else:
                benchs = [(group,val) for group,(gpath,gbenchs) in allBenchs.items() for val in gbenchs.keys() ]
            super().__init__(stdscr, benchs, cols ,config["nruns"])
            self.config = {**config }
            if not os.path.exists("logs"): os.makedirs("logs")
    
    def run_benchmark(self, group, benchmark, cls):
        logfile = "logs/"+benchmark+cls
        cwd = os.getcwd()
        with open(logfile,"w+") as f:
            if self.config["col"] and not (cls == self.config["col"]): return skippedRes
            if self.config["row"] and not (benchmark == self.config["row"]): return skippedRes
            try:
                if ("HyperLasso" in cls):
                    witness = "--witness=true" if "+w" in cls else "--witness=false"
                    params = witness + " --complete=nuxmv "
                    cfg = {**self.config, "tool": "HyperLasso", "params" : params, "logfile" : f, "path": getPath(group) }
                    bench = getBenchs(group)[benchmark]
                    if benchmark in timeouts[cls] and cfg["skipTimeout"]:
                        printLog("skipped benchmark that times out",f)
                        res = skippedRes
                    elif (not bench["expectsWitness"]) and cfg["skipIncomplete"]:
                        printLog("skipped benchmark that is incomplete",f)
                        res = skippedRes
                    else: res = benchHyperLasso(cfg,benchmark,bench)
                if ("AutoHyper" in cls):
                    params = " --witness --incl-forklift " if "+w" in cls else " --incl-forq "
                    witness = "+w" in cls
                    cfg = {**self.config, "tool": "AutoHyper", "params" : params, "witness": witness, "logfile" : f, "path": getPath(group) }
                    bench = getBenchs(group)[benchmark]
                    if benchmark in timeouts[cls] and cfg["skipTimeout"]:
                        printLog("skipped benchmark that times out",f)
                        res = skippedRes
                    elif (not bench["expectsWitness"]) and cfg["skipIncomplete"]:
                        printLog("skipped benchmark that is incomplete",f)
                        res = skippedRes
                    else: res = benchAutoHyper(cfg,benchmark,bench)
            except Exception as e:
                printLog(e,f)
                os.chdir(cwd)
                return errorRes
        
        return res

class CLI(object):
    """HyperLasso Benchmarking"""
    
    def run(self,nruns=1,group=None,row=None,col=None,debug=False,timeout=300,skipTimeout=True,skipIncomplete=False):
        """
        Executes the benchmark suite and displays results in a GUI.
        
        Args:
            nruns: Number of iterations to run for each individual benchmark. Average times are reported.
            group: Filter benchmarks by a specific group name. Supported options ('Paper','CMS','SelfStabilization','CNI','Planning','Isolation','Controller','Bakery').
            row: Run only a specific benchmark row by name.
            col: Run only a specific tool/column. Supported options ('AutoHyper-w' for AutoHyper without witness finding, 'AutoHyper+w' for AutoHyper with witness finding, 'HyperLasso+w' for HyperLasso with witness finding).
            debug: If True, prints internal debugging information to logs in folder 'logs'.
            timeout: Maximum seconds allowed per benchmark invocation.
            skipTimeout: Skip benchmarks known to exceed the timeout.
            skipIncomplete: Skip benchmarks that cannot produce a witness.
        """
    
        cfg = {"nruns": nruns, "group" : group, "row" : row, "col" : col, "debug" : debug, "timeout" : timeout, "skipTimeout" : skipTimeout, "skipIncomplete" : skipIncomplete }
    
        def go(stdscr):
            curses.curs_set(0)
            gui = DoBenchmarkGUI(stdscr, cfg)
            gui.run_all()
        curses.wrapper(go)

        atexit.register(cleanup_all)

if __name__ == '__main__':
  fire.Fire(CLI().run)
  
import sys

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) != 2:
        print("Usage: python bakery.py <processes>")
        sys.exit(1)

    N = int(sys.argv[1])  # number of processes

    print("MODULE main")
    print("VAR")
    print(f"    i : 0..{N};")
    for i in range(N):
        print(f"    pc_{i} : 0..5;")  # 0: non-critical, 1: determine maximum, 2: draw number, 3: trying, 4: critical, 5: exit
        print(f"    number_{i} : 0..{N};")  # ticket number
        print(f"    tmp_{i} : 0..{N};")  # temporary variable so simulate no atomic maximum calculation

    print("ASSIGN")

    print("    init(i) := {" + ",".join(str(i) for i in range(N+1)) + "};")
    print("    next(i) := {" + ",".join(str(i) for i in range(N+1)) + "};")

    def parens(s):
        if s: return "(" + s + ")"
        else: return s

    def rand(x):
        if x: return " & " + x
        else: return x

    def anot(s):
        if s: return "!" + s
        else: return ""

    def has_max_number(i):
        stmts = []
        for j in range(N):
            if j != i:
                stmts.append(f"number_{j} <= number_{i}")
        return " & ".join(stmts)
    
    def can_enter(i):
        stmts = []
        for j in range(N):
            if j != i:
                stmts.append(f"(number_{j} = 0 | number_{i} < number_{j} | (number_{i} = number_{j} & {i} < {j}))")
                stmts.append(f"!(pc_{j} = 2)")
        return parens(" & ".join(stmts))

    for i in range(N):
        print(f"    init(pc_{i}) := 0;")
        print(f"    init(number_{i}) := 0;")
        print(f"    init(tmp_{i}) := {{" + ",".join(str(k) for k in range(N+1)) + "};")

        cases = []
        cases.append(f"(i = {i} & pc_{i} = 0): 1;")
        cases.append(f"(i = {i} & pc_{i} = 1): 2;")
        cases.append(f"(i = {i} & pc_{i} = 2 & tmp_{i} < {N}): 3;")
        cases.append(f"(i = {i} & pc_{i} = 3 " + rand(can_enter(i)) + "): 4;")
        cases.append(f"(i = {i} & pc_{i} = 4): 5;")
        cases.append(f"(i = {i} & pc_{i} = 5): 0;")
        cases.append(f"TRUE: pc_{i};")
        print(f"    next(pc_{i}) := case " + " ".join(cases) + " esac;")
        print(f"    next(number_{i}) := case (i = {i} & pc_{i} = 2 & tmp_{i} < {N}): tmp_{i} + 1; (i = {i} & pc_{i} = 5) : 0; TRUE: number_{i}; esac;")
        cases = []
        for j in range(N):
            cases.append(f"(i = {i} & pc_{i} = 1 " + rand(has_max_number(j)) + f"): number_{j};")
        cases.append(f"TRUE: tmp_{i};")
        print(f"    next(tmp_{i}) := case " + " ".join(cases) + " esac;")  
    exit()

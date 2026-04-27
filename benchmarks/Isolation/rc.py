import sys

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) != 3:
        print("Usage: python isolation_rc_assigns.py <transactions> <keys>")
        sys.exit(1)

    T = int(sys.argv[1])  # number of transactions
    K = int(sys.argv[2])  # number of keys and values
    V = K

    print("MODULE main")

    print("VAR")

    for t in range(T):
        for k in range(K):
            print(f"    reads_{t}_{k} : 0..{V};")
            print(f"    writes_{t}_{k} : 0..{V};")

    print(f"    t : 0..{T};")

    for t in range(T):
        print(f"    installed_{t} : boolean;")
    
    for k in range(K):
        print(f"    value_{k} : 0..{V};")

    print("ASSIGN")
    
    for t in range(T):
        for k in range(K):
            print(f"    next(reads_{t}_{k}) := reads_{t}_{k};")
            print(f"    next(writes_{t}_{k}) := writes_{t}_{k};")

    def can_install(t):
        stmts = [f"t = {t}", f"!installed_{t}"]
        for k in range(K):
            stmts.append(f"(reads_{t}_{k} = 0 | (" + " | ".join([f"reads_{t}_{k} = writes_{t2}_{k} & installed_{t2}" for t2 in range(T) if t2 != t]) + "))")
        return "(" + " & ".join(stmts) + ")"

    for t in range(T):
        print(f"    init(installed_{t}) := FALSE;")
        print(f"    next(installed_{t}) := case (" + can_install(t) + f"): TRUE; TRUE: installed_{t}; esac;")
    for k in range(K):
        print(f"    init(value_{k}) := 0;")
        cases = []
        for t in range(T):
            cases.append(f"(" + can_install(t) + f" & writes_{t}_{k} > 0): writes_{t}_{k};")
        cases.append(f"TRUE: value_{k};")
        print(f"    next(value_{k}) := case " + " ".join(cases) + " esac;")



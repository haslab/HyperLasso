import sys

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) != 3:
        print("Usage: python cms_deterministic_assigns.py <reviewers> <articles>")
        sys.exit(1)

    R = int(sys.argv[1])  # number of reviewers
    A = int(sys.argv[2])  # number of articles

    print("MODULE main")

    print("VAR")

    for a in range(A):
        for r in range(R):
            print(f"    assigns_{a}_{r} : boolean;")

    print(f"    a : 0..{A};") # a = A -> stutter
    print(f"    r : 0..{R};") # r < R -> review, r = R -> decide
    print("    d : 1..3;") # decision value
    
    # possible reviews: 0 (no review), 1 (reject), 2 (major revision), 3 (accept)
    
    for a in range(A):
        for r in range(R):
            print(f"    review_{a}_{r} : 0..3;")
    
    for a in range(A):
        print(f"    decision_{a} : 0..3;")


    print("ASSIGN")
    
    for a in range(A):
        for r in range(R):
            print(f"    next(assigns_{a}_{r}) := assigns_{a}_{r};")

    def can_review(a,r):
        stmts = []
        stmts.append(f"r = {r}")
        stmts.append(f"a = {a}")
        stmts.append(f"assigns_{a}_{r}")
        stmts.append(f"review_{a}_{r} != d")
        stmts.append(f"decision_{a} = 0")
        return "(" + " & ".join(stmts) + ")"

    for a in range(A):
        for r in range(R):
            print(f"    init(review_{a}_{r}) := 0;")
            print(f"    next(review_{a}_{r}) := case " + can_review(a,r) + f": d; TRUE: review_{a}_{r}; esac;")

    def can_decide(a):
        stmts = []
        stmts.append(f"r = {R}")
        stmts.append(f"a = {a}")
        stmts.append(f"decision_{a} = 0")
        for r in range(R):
            stmts.append(f"(assigns_{a}_{r} -> review_{a}_{r} != 0)")
        stmts.append("(" + " | ".join([f"review_{a}_{r} = d" for r in range(R)]) + ")")
        stmts.append("(" + " & ".join([f"review_{a}_{r} <= d" for r in range(R)]) + ")")
        return "(" + " & ".join(stmts) + ")"
    
    for a in range(A):
        print(f"    init(decision_{a}) := 0;")
        print(f"    next(decision_{a}) := case " + can_decide(a) + f": d; TRUE: decision_{a}; esac;")
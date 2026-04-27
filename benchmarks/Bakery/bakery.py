import sys

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) != 2:
        print("Usage: python bakery.py <processes>")
        sys.exit(1)

    N = int(sys.argv[1])  # number of processes

    print("MODULE main")
    print("VAR")
    for i in range(N):
        print(f"    pc_{i} : 0..5;")  # 0: non-critical, 1: determine maximum, 2: draw number, 3: trying, 4: critical, 5: exit
        print(f"    number_{i} : 0..{N};")  # ticket number
        print(f"    tmp_{i} : 0..{N};")  # temporary variable so simulate no atomic maximum calculation

    print("INIT")
    init_conditions = []
    for i in range(N):
        init_conditions.append(f"pc_{i} = 0 & number_{i} = 0")
    print("    " + " & ".join(init_conditions))

    def parens(s):
        if s: return "(" + s + ")"
        else: return s

    def rand(x):
        if x: return " & " + x
        else: return x

    def anot(s):
        if s: return "!" + s
        else: return "FALSE"

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

    def process(i):
        stmts = []
        stmts.append(f"pc_{i} = 0 & next(pc_{i}) = 1 & next(number_{i}) = number_{i} & next(tmp_{i}) = tmp_{i}")  # non-critical to draw number
        for j in range(N):
            stmts.append(f"pc_{i} = 1 " + rand(has_max_number(j)) + f" & next(pc_{i}) = 2 & next(number_{i}) = number_{i} & next(tmp_{i}) = number_{j}")  # determine maximum
        stmts.append(f"pc_{i} = 2 & next(pc_{i}) = 3 & next(number_{i}) = tmp_{i} + 1 & next(tmp_{i}) = tmp_{i}")  # draw number    
        stmts.append(f"pc_{i} = 3 " + rand(can_enter(i)) + f" & next(pc_{i}) = 4 & next(number_{i}) = number_{i} & next(tmp_{i}) = tmp_{i}")  # trying to critical
        stmts.append(f"pc_{i} = 3 " + rand(anot(can_enter(i))) + f" & next(pc_{i}) = 3 & next(number_{i}) = number_{i} & next(tmp_{i}) = tmp_{i}")  # trying to critical
        stmts.append(f"pc_{i} = 4 & next(pc_{i}) = 5 & next(number_{i}) = number_{i} & next(tmp_{i}) = tmp_{i}")  # critical to exit
        stmts.append(f"pc_{i} = 5 & next(pc_{i}) = 0 & next(number_{i}) = 0 & next(tmp_{i}) = tmp_{i}")  # exit to non-critical
        trans = "(" + " | ".join(stmts) + ")"
        for j in range(N):
            if j != i:
                trans += f" & next(pc_{j}) = pc_{j} & next(number_{j}) = number_{j} & next(tmp_{j}) = tmp_{j}"
        return parens(trans)

    def stutter():
        stmts = []
        for i in range(N):
            stmts.append(f"next(pc_{i}) = pc_{i}")
            stmts.append(f"next(number_{i}) = number_{i}")
            stmts.append(f"next(tmp_{i}) = tmp_{i}")
        return parens(" & ".join(stmts))

    print("TRANS")

    for i in range(N):
        print("    " + process(i) + " |")
    print("    " + stutter())


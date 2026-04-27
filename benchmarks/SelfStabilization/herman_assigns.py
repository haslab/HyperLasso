import sys

def parens(s):
    if s: return "(" + s + ")"
    else: return s

def ands(xs):
    if xs: return " & ".join(xs)
    else: return "TRUE"
    
def ors(xs):
    if xs: return " | ".join(xs)
    else: return "FALSE"

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) != 2:
        print("Usage: python herman_assigns.py <tokens>")
        sys.exit(1)

    N = int(sys.argv[1])  # number of tokens

    print("MODULE main")
    print("VAR")
    
    # N processes
    for i in range(N):
        print(f"    token{i} : boolean;") 

    # Scheduler
    print(f"    sched : 0..{N-1};")
    
    # Protocol (coin flips: keep token = FALSE, pass token = TRUE)
    print(f"    coin : boolean;")

    def assert_token(i):
        stmts = []
        for j in range(N):
            if i==j: stmts.append(f"token{j}")
            else: stmts.append(f"!token{j}")
        return parens(ands(stmts))

    print("DEFINE")
    print(f"    stable := " + ors([ assert_token(i) for i in range(N) ]) + ";")
    
    # Transition logic: Processes pass token in a Ring
    print("ASSIGN")
    for i in range(N):
        j = N-1 if i==0 else i-1
        print(f"    next(token{i}) := case")
        # send token
        print(f"        sched = {i} & token{i} & coin : FALSE;")
        # keep token
        print(f"        sched = {i} & token{i} & !coin : FALSE;")
        # receive token
        print(f"        sched = {j} & token{j} & coin : TRUE;")
        # stutter
        print(f"        TRUE : token{i};")
        print(f"    esac;")
    










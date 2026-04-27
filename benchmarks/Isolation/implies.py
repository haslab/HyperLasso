import sys
from render_formula import *

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) < 3:
        print("Usage: python implies.py <transactions> <keys> <ah>")
        sys.exit(1)

    T = int(sys.argv[1])  # number of transactions
    K = int(sys.argv[2])  # number of keys and values
    AH = len(sys.argv) > 3 and sys.argv[3] =="ah"

    def same_transactions(A,B):
        stmts = []
        for t in range(T):
            for k in range(K):
                stmts.append( ands([v(f"reads_{t}_{k}",A) == v(f"reads_{t}_{k}",B),v(f"writes_{t}_{k}",A) == v(f"writes_{t}_{k}",B)],isLTL=True) )
        return ands(stmts)
    
    def all_installed(A):
        stmts = []
        for t in range(T):
            stmts.append(atomic(f"installed_{t}",A))
        return F(ands(stmts))
    
    A = "A"
    B = "B"
    spec = forall(A,exists(B, all_installed(A) >> ( same_transactions(A,B) & all_installed(B) ) ))
    spec.prettyprint(ah=AH)
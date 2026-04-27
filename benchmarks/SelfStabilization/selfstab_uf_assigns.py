import sys
from render_formula import *

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) < 2:
        print("Usage: python selfstab_uf.py <tokens> <ah>")
        sys.exit(1)

    T = int(sys.argv[1])
    AH = len(sys.argv) > 2 and sys.argv[2] =="ah"
    
    A = "A"
    B = "B"
    
    same_tokens = [ equiv(atomic(f"token{i}",A),atomic(f"token{i}",B)) for i in range(T) ]
    schedule = G(v("sched",A) == v("sched",B))
    stable = F(G(v("stable",B)))
    
    def inits(M):
        return ors([ atomic(f"token{i}",M) for i in range(T) ])
    
    props = same_tokens + [schedule,stable]
    spec = forall(A,exists(B,inits(A) >> ands([inits(B)] + props)))
    spec.prettyprint(ah=AH)

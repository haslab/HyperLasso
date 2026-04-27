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
    
    same_tokens = [ v(f"token{i}",A) == v(f"token{i}",B) for i in range(T) ]
    schedule = G(v("sched",A) == v("sched",B))
    stable = F(G(v("stable",B)))
    
    props = same_tokens + [schedule,stable]
    spec = forall(A,exists(B,ands(props)))
    spec.prettyprint(ah=AH)

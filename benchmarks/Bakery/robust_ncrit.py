import sys
from render_formula import *

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) < 2:
        print("Usage: python robust_ncrit.py <processes> <ah>")
        sys.exit(1)

    P = int(sys.argv[1])
    AH = len(sys.argv) > 2 and sys.argv[2] =="ah"
    
    A = "A"
    B = "B"
    
    same_pcs = [ v(f"pc_{i}",A) == v(f"pc_{i}",B) for i in range(P) ]
    same_numbers = [ v(f"pc_{i}",A) == v(f"pc_{i}",B) for i in range(P) ]
    fixed_process = G(v("pc_0",A) == v("pc_0",B))
    other_processes = [ G(F(v(f"pc_{i}",B) == const(4))) for i in range(1,P) ]
    
    fair = F(G(v("pc_0",A) == const(0)))
    props = same_pcs + same_numbers + [fixed_process] + other_processes
    spec = forall(A,exists(B,fair >> ands(props)))
    spec.prettyprint(ah=AH)



    
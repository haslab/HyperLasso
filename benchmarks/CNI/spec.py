import sys
from render_formula import *

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) < 2:
        print("Usage: python spec.py <size> <ah>")
        sys.exit(1)

    S = int(sys.argv[1])  # bitstring size
    AH = len(sys.argv) > 2 and sys.argv[2] =="ah"
    
    A = "A"
    B = "B"
    
    assumption = ands([ G( F( v("turn",A) == const(i) ) ) for i in range(3) ],isLTL=True)
    
    p1 = ~(ands([ equiv(atomic(f"PIN_{i}",A) , atomic(f"PIN_{i}",B)) for i in range(S) ]))
    p21 = [ ~atomic(f"MASK_{i}",A) for i in range(S) ]
    p22 = [ ~atomic(f"MASK_{i}",B) for i in range(S) ]
    p23 = [ equiv(atomic(f"RESULT_{i}",A) , atomic(f"RESULT_{i}",B)) for i in range(S) ]
    p2 = F(ands(p21 + p22 + p23))    
    spec = forall(A, exists(B, assumption >> (p1 & p2) ))
    spec.prettyprint(ah=AH)

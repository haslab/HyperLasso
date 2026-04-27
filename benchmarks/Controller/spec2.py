
import sys
from render_formula import *

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) < 3:
        print("Usage: python spec2.py <states> <transitions> <ah>")
        sys.exit(1)

    S = int(sys.argv[1])  # number of states
    T = int(sys.argv[2])  # number of transitions
    AH = len(sys.argv) > 3 and sys.argv[3] =="ah"
    
    def transition(C,M,t):
        stmts = []
        stmts.append(v("state",M) == v(f"from_{t}",C))
        stmts.append(v(f"button_{t}_left",C) == v("button_left",M))
        stmts.append(v(f"button_{t}_right",C) == v("button_right",M))
        stmts.append(v("green_left",M) == v(f"green_{t}_left",C))
        stmts.append(v("green_right",M) == v(f"green_{t}_right",C))
        stmts.append(X(v("state",M) == v(f"to_{t}",C)))    
        return ands(stmts)

    def default(C,M,s):
        stmts = []
        stmts.append(v("state",M) == v(s))
        for t in range(T):
            stmts.append(ors(
                [ v(f"from_{t}",C) != v("state",M)
                , v(f"button_{t}_left",C) != v(f"button_left",M)
                , v(f"button_{t}_right",C) != v(f"button_right",M)
                ]))
        stmts.append(v("green_left",M) == v(f"default_green_left_{s}",C))
        stmts.append(v("green_right",M) == v(f"default_green_right_{s}",C))
        stmts.append(X(v("state",M) == v(f"default_to_{s}",C)))
        return ands(stmts)
    
    def behavior(C,M):
        b1 = v("state",M) == v("0")
        b2 = G(ors([ transition(C,M,t) for t in range(T) ] + [ default(C,M,s) for s in range(S) ]))
        return b1 & b2

    def mealy(C):
        constraints = []
        for t1, t2 in itertools.combinations(range(T), 2):
            left = v(f"from_{t1}",C) == v(f"from_{t2}",C)
            right = (v(f"button_{t1}_left",C) != v(f"button_{t2}_left",C)) | (v(f"button_{t1}_right",C) != v(f"button_{t2}_right",C))
            constraints.append(left >> right)
        return ands(constraints)

    def assumption(M):
        a1 = G(v("button_left",M) >> X(~v("button_left",M)))
        a2 = G(v("button_right",M) >> X(~(v("button_right",M))))
        a3 = G(F(v("button_left",M)))
        a4 = G(F(v("button_right",M)))
        return ands([a1,a2,a3,a4])
    
    def guarantee(M):
        g1 = G(~v("green_left",M) | ~v("green_right",M))
        g2 = G(v("button_left",M) >> F(v("green_left",M)))
        g3 = G(v("button_right",M) >> F(v("green_right",M)))
        return ands([g1,g2,g3])
    
    C = "C"
    M = "M"
    spec = exists(C,forall(M,mealy(C) & ((assumption(M) & behavior(C,M)) >> guarantee(M)) ))
    spec.prettyprint(ah=AH)
    

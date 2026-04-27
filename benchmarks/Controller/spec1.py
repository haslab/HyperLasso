
import sys
from render_formula import *

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python spec1.py <states> <transitions> <ah>")
        sys.exit(1)

    S, T = int(sys.argv[1]), int(sys.argv[2])
    AH = len(sys.argv) > 3 and sys.argv[3] =="ah"

    def transition(C, M, t):
        return ands([
            v("state",M) == v(f"from_{t}",C),
            equiv(v(f"button_{t}_left",C) , v("button_left",M)),
            equiv(v(f"button_{t}_right",C) , v("button_right",M)),
            equiv(v("green_left",M) , v(f"green_{t}_left",C)),
            equiv(v("green_right",M) , v(f"green_{t}_right",C)),
            X(v("state",M) == v(f"to_{t}",C))
        ])

    def default(C, M, s):
        stmts = [v("state",M) == v(s)]
        for t in range(T):
            stmts.append(~(ands(
                [ v(f"from_{t}",C) == v("state",M)
                , equiv(v(f"button_{t}_left",C) , v("button_left",M))
                , equiv(v(f"button_{t}_right",C) , v("button_right",M))
                ]))
            )
        stmts.extend(
            [ equiv(v("green_left",M) , v(f"default_green_left_{s}",C))
            , equiv(v("green_right",M) , v(f"default_green_right_{s}",C))
            , X(v("state",M) == v(f"default_to_{s}",C))])
        return ands(stmts)

    def behavior(C, M):
        return (v("state",M) == v("0")) & G(ors([transition(C, M, t) for t in range(T)] + [default(C, M, s) for s in range(S)]))

    def mealy(C):
        constraints = []
        for t1, t2 in itertools.combinations(range(T), 2):
            left = v(f"from_{t1}",C) == v(f"from_{t2}",C)
            r1 = ~(equiv(v(f"button_{t1}_left",C) , v(f"button_{t2}_left",C)))
            r2 = ~(equiv(v(f"button_{t1}_right",C) , v(f"button_{t2}_right",C)))
            right = r1 | r2
            constraints.append(left >> right)
        return ands(constraints)

    def assumption(M):
        return G(F(atomic("button_left",M))) & G(F(atomic("button_right",M)))

    def guarantee(M):
        return ands(
            [ G(~atomic("green_left",M) | ~atomic("green_right",M))
            , G(atomic("button_left",M) >> F(atomic("green_left",M)))
            , G(atomic("button_right",M) >> F(atomic("green_right",M)))
            , G((~atomic("green_left",M) & X(atomic("green_left",M))) >> X(X(atomic("green_left",M))))
            , G((~atomic("green_right",M) & X(atomic("green_right",M))) >> X(X(atomic("green_right",M))))
            ])

    C = "C"
    M = "M"
    spec = exists(C, forall(M, mealy(C) & ((assumption(M) & behavior(C,M)) >> guarantee(M)) ))
    spec.prettyprint(ah=AH)   
    

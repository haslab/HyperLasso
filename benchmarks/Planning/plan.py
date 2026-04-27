import sys
from render_formula import *

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) < 3:
        print("Usage: python plan.py <size> <enemies> <ah>")
        sys.exit(1)

    S = int(sys.argv[1])  # map size
    E = int(sys.argv[2])  # number of enemies
    AH = len(sys.argv) > 3 and sys.argv[3] =="ah"

    def keep_moving(A):
        stmts = []
        for x in range(S):
            for y in range(S):
                stmts.append(G(F((v("x",A) != const(x)) | (v("y",A) != const(y)))))
        return ands(stmts) 
    
    def avoids_enemies(R,A):
        return G((v("x",R) != v("x",A)) | (v("y",R) != v("y",A)))
    
    def start_position(A,x,y):
        return (v("x",A) == const(x)) & (v("y",A) == const(y))
    
    def stay_in_row(A,y):
        return G(v("y",A) == const(y))
    
    def propi(i): return (start_position(f"A{i}",S-1,i) & stay_in_row(f"A{i}",i)) >> avoids_enemies("A",f"A{i}")
    prop = ands([keep_moving("A") , start_position("A",0,0)] + [ propi(i) for i in range(E) ])
    spec = exists("A",forallN("A",E,prop))
    spec.prettyprint(ah=AH)


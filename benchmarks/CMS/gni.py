import sys
from render_formula import *

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) < 3:
        print("Usage: python gni.py <reviewers> <articles> <ah>")
        sys.exit(1)

    R = int(sys.argv[1])  # number of reviewers
    A = int(sys.argv[2])  # number of articles
    AH = len(sys.argv) > 3 and sys.argv[3] =="ah"

    def same_assigns(x,T,U):
        stmts = []
        for a in range(A):
            for r in range(R):
                stmts.append(( atomic(f"assigns_{a}_{x}",T) | atomic(f"assigns_{a}_{x}",U) ) >> equiv(atomic(f"assigns_{a}_{r}",T) , atomic(f"assigns_{a}_{r}",U)))
        return ands(stmts,isLTL=True)

    def same_assigns_others(x,T,U):
        stmts = []
        for a in range(A):
            for r in range(R):
                stmts.append(~(atomic(f"assigns_{a}_{x}",T) | atomic(f"assigns_{a}_{x}",U) ) >> equiv( atomic(f"assigns_{a}_{r}",T) , atomic(f"assigns_{a}_{r}",U) ))
        return ands(stmts,isLTL=True)

    def aligned(r,T,U):
        stmts = []
        for a in range(A):
            s1 = equiv(v("a",T) == const(a),v("a",U) == const(a))
            s2 = (v("a",T) == const(a)) >> (v("r",T) == v("r",U))
            stmts.append(atomic(f"assigns_{a}_{r}",T) >> ( ands([s1,s2],isLTL=True) ) )
        return G(ands(stmts,isLTL=True))

    def all_decided(T):
        stmts = []
        for a in range(A):
            stmts.append(v(f"decision_{a}",T) != const(0))
        return F(ands(stmts,isLTL=True))

    def same_reviews_decisions(r,T,U):
        stmts = []
        for a in range(A):
            same_decisions = v(f"decision_{a}",T) == v(f"decision_{a}",U)
            same_reviews = [v(f"review_{a}_{r2}",T) == v(f"review_{a}_{r2}",U) for r2 in range(R)]
            stmts.append( atomic(f"assigns_{a}_{r}",T) >> ands([same_decisions] + same_reviews,isLTL=True) )
        return G(ands(stmts,isLTL=True))
    
    def same_reviews_others(r,T,U):
        stmts = []
        for a in range(A):
            same_reviews = [v(f"review_{a}_{r2}",T) == v(f"review_{a}_{r2}",U) for r2 in range(R)]
            stmts.append(~(atomic(f"assigns_{a}_{r}",T)) >> ands(same_reviews,isLTL=True))
        return G(ands(stmts,isLTL=True))

    def can_review(a,r,T):
        stmts = []
        stmts.append(v("r",T) == const(r))
        stmts.append(v("a",T) == const(a))
        stmts.append(atomic(f"assigns_{a}_{r}",T))
        stmts.append(v(f"review_{a}_{r}",T) != v("d",T))
        stmts.append(v(f"decision_{a}",T) == const(0))
        return (ands(stmts,isLTL=True))

    def enabled_reviews(T):
        stmts = []
        for a in range(A):
            for r in range(R):
                pre = ands([v("a",T) == const(a) , v("r",T) == const(r)],isLTL=True)
                stmts.append(pre >> can_review(a,r,T))
        return G(ands(stmts,isLTL=True))
    
    def valid_assigns(T):
        stmts = []
        for a in range(A):
            stmts.append(ors([atomic(f"assigns_{a}_{r}",T) for r in range(R)]))    
        return (ands(stmts,isLTL=True))
    
    assumptions = ands([ valid_assigns("A") , valid_assigns("B") , aligned(0,"A","B") , all_decided("A") , all_decided("B") , enabled_reviews("A") , enabled_reviews("B") ])
    prop = ands([ all_decided("C") , same_assigns(0,"A","C") , same_reviews_decisions(0,"A","C") , same_assigns_others(0,"B","C") , same_reviews_others(0,"B","C") ])
    
    spec = forall("A", forall("B", exists("C", assumptions >> prop ) ))
    spec.prettyprint(ah=AH)
    


    
    
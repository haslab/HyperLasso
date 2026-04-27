import sys

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) != 3:
        print("Usage: python cms_gni.py <reviewers> <articles>")
        sys.exit(1)

    R = int(sys.argv[1])  # number of reviewers
    A = int(sys.argv[2])  # number of articles

    def same_assigns(x,T,U):
        stmts = []
        for a in range(A):
            for r in range(R):
                stmts.append(f'(({{"assigns_{a}_{x}"_{T}}} | {{"assigns_{a}_{x}"_{U}}}) -> {{"assigns_{a}_{r}"_{T}}} = {{"assigns_{a}_{r}"_{U}}})')
        return "(" + " & ".join(stmts) + ")"

    def same_assigns_others(x,T,U):
        stmts = []
        for a in range(A):
            for r in range(R):
                stmts.append(f'(!({{"assigns_{a}_{x}"_{T}}} | {{"assigns_{a}_{x}"_{U}}}) -> {{"assigns_{a}_{r}"_{T} = "assigns_{a}_{r}"_{U}}})')
        return "(" + " & ".join(stmts) + ")"

    def aligned(r,T,U):
        stmts = []
        for a in range(A):
            stmts.append(f'({{"assigns_{a}_{r}"_{T}}} -> ((({{"a"_{T} = {a}}}) <-> ({{"a"_{U} = {a}}})) & (({{"a"_{T} = {a}}}) -> ({{"r"_{T} = "r"_{U}}}))))')
        return "G (" + " & ".join(stmts) + ")"

    def all_decided(T):
        stmts = []
        for a in range(A):
            stmts.append(f'{{"decision_{a}"_{T} != 0}}')
        return "F (" + " & ".join(stmts) + ")"

    def same_reviews_decisions(r,T,U):
        stmts = []
        for a in range(A):
            stmts.append(f'({{"assigns_{a}_{r}"_{T}}} -> ({{"decision_{a}"_{T} = "decision_{a}"_{U}}} & ' + " & ".join([f'{{"review_{a}_{r2}"_{T} = "review_{a}_{r2}"_{U}}}' for r2 in range(R)]) + "))")
        return "G (" + " & ".join(stmts) + ")"
    
    def same_reviews_others(r,T,U):
        stmts = []
        for a in range(A):
            stmts.append(f'(!{{"assigns_{a}_{r}"_{T}}} -> (' + " & ".join([f'{{"review_{a}_{r2}"_{T} = "review_{a}_{r2}"_{U}}}' for r2 in range(R)]) + "))")
        return "G (" + " & ".join(stmts) + ")"

    def can_review(a,r,T):
        stmts = []
        stmts.append(f'{{"r"_{T} = {r}}}')
        stmts.append(f'{{"a"_{T} = {a}}}')
        stmts.append(f'{{"assigns_{a}_{r}"_{T}}}')
        stmts.append(f'{{"review_{a}_{r}"_{T} != "d"_{T}}}')
        stmts.append(f'{{"decision_{a}"_{T} = 0}}')
        return "(" + " & ".join(stmts) + ")"

    def enabled_reviews(T):
        stmts = []
        for a in range(A):
            for r in range(R):
                stmts.append(f'((({{"a"_{T} = {a}}}) & ({{"r"_{T} = {r}}})) -> ' + can_review(a,r,T) + ")")
        return "G (" + " & ".join(stmts) + ")"
    
    def valid_assigns(T):
        stmts = []
        for a in range(A):
            stmts.append("(" + " | ".join([f'{{"assigns_{a}_{r}"_{T}}}' for r in range(R)]) + ")")
        return "(" + " & ".join(stmts) + ")"

    print("forall A. forall B. exists C.\n    (" + valid_assigns("A") + " & " + valid_assigns("B") + " & " + aligned(0,"A","B") + " & " + all_decided("A") + " & " + all_decided("B") + " & " + enabled_reviews("A") + " & " + enabled_reviews("B") + ")\n    ->\n    (" + all_decided("C") + " & " + same_assigns(0,"A","C") + " & " + same_reviews_decisions(0,"A","C") + " & " + same_assigns_others(0,"B","C") + " & " + same_reviews_others(0,"B","C") + ")")

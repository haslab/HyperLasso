import sys

def model(S,C):
    body = ["MODULE main",""]
    body += ["", "VAR"]  
    body += [f"PIN_{i} : boolean;" for i in range(S)]
    body += [f"MASK_{i} : boolean;" for i in range(S)]
    body += [f"RESULT_{i} : boolean;" for i in range(S)]
    body += ["""
trigger_0 : boolean;
trigger_1 : boolean;
trigger_2 : 0..2;

pc_0 : 0..2;
pc_1 : 0..2;
pc_2 : 0..3;

turn : 0..2;
"""]
    
    if C:
        body += ["INIT"]
        lhs = " + ".join(f"((PIN_{i}?1:0)*{2**i})" for i in range(S))
        rhs = " + ".join(f"((MASK_{i}?1:0)*{2**i})" for i in range(S))
        body += [f"({lhs}) > ({rhs});"]
    
    body += ["", "ASSIGN"]

    body += [f"init(RESULT_{i}) := FALSE;" for i in range(S)]

    body += ["""
init(pc_0) := 0;
init(pc_1) := 0;
init(pc_2) := 0;

init(trigger_0) := FALSE;
init(trigger_1) := FALSE;
init(trigger_2) := 0;"""]

    body += [f"next(PIN_{i}) := PIN_{i};" for i in range(S)]

    body += [f"""
next(RESULT_{i}) :=
    case
        turn = 0 & pc_0 = 2 : RESULT_{i} | MASK_{i};  -- bitwise or
        turn = 1 & pc_1 = 2 : RESULT_{i} & !MASK_{i}; -- bitwise and with mask complement
        TRUE : RESULT_{i};
    esac;""" for i in range(S)]        

    body += [f"""
next(MASK_{i}) :=
    case
        turn = 2 & pc_2 = 3 : {"MASK_"+str(i+1) if i+1 < S else "FALSE"}; -- divide by 2
        TRUE : MASK_{i};
    esac;""" for i in range(S)]

    cond = " & ".join(f"(PIN_{i} = MASK_{i})" for i in range(S))

    body += [f"""
next(trigger_0) :=
    case
        turn = 0 & pc_0 = 2 : FALSE;
        turn = 1 & pc_1 = 2 & trigger_2 = 0 : TRUE;
        turn = 2 & pc_2 = 1 & ({cond}) : TRUE;
        TRUE : trigger_0;
    esac;

next(trigger_1) :=
    case
        turn = 0 & pc_0 = 2 & trigger_2=0 : TRUE;
        turn = 1 & pc_1 = 2 : FALSE;
        turn = 2 & pc_2 = 1 & !({cond}) : TRUE;
        TRUE : trigger_1;
    esac;

next(trigger_2) :=
    case
        turn = 0 & pc_0 = 2 & trigger_2 < 2 : trigger_2+1;
        turn = 1 & pc_1 = 2 & trigger_2 < 2 : trigger_2+1;
        turn = 2 & pc_2 = 1 : 0;
        TRUE : trigger_2;
    esac;"""]    

    cond = " | ".join(f"MASK_{i}" for i in range(S))

    body += [f"""
next(pc_0) :=
    case
        turn = 0 & pc_0 = 0 & !({cond}) : pc_0;
        turn = 0 & pc_0 = 0 &  ({cond}) : pc_0+1;
        turn = 0 & pc_0 = 1 & !trigger_0 : pc_0;
        turn = 0 & pc_0 = 1 &  trigger_0 : pc_0+1;
        turn = 0 & pc_0 = 2 : 0;
        TRUE : pc_0;
    esac;

next(pc_1) :=
    case
        turn = 1 & pc_1=0 & !({cond}) : pc_1;
        turn = 1 & pc_1=0 &  ({cond}) : pc_1+1;
        turn = 1 & pc_1=1 & !trigger_1 : pc_1;
        turn = 1 & pc_1=1 &  trigger_1 : pc_1+1;
        turn = 1 & pc_1=2 : 0;
        TRUE : pc_1;
    esac;

next(pc_2) :=
    case
        turn = 2 & pc_2=0 & !({cond}) : pc_2;
        turn = 2 & pc_2=0 &  ({cond}) : pc_2+1;
        turn = 2 & pc_2=1 : pc_2+1;
        turn = 2 & pc_2=2 & !(trigger_2=2) : pc_2;
        turn = 2 & pc_2=2 &  (trigger_2=2) : pc_2+1;
        turn = 2 & pc_2=3 : 0;
        TRUE : pc_2;
    esac;
"""]

    return "\n".join(body)

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) != 3 or not sys.argv[2] in ["any","lte"]:
        print("Usage: python cni.py <size> <version>")
        sys.exit(1)

    S = int(sys.argv[1])  # bitstring size
    C = sys.argv[2] == "lte"  # any or lte version

    sys.stdout.write(model(S,C))

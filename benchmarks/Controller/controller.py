import sys

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) != 3:
        print("Usage: python controller.py <states> <transitions>")
        sys.exit(1)

    S = int(sys.argv[1])  # number of states
    T = int(sys.argv[2])  # number of transitions

    print("MODULE main")
    print("VAR")

    for t in range(T):
        print(f"    from_{t} : 0..{S-1};")
        print(f"    to_{t} : 0..{S-1};")
        print(f"    button_{t}_left : boolean;")
        print(f"    button_{t}_right : boolean;")
        print(f"    green_{t}_left : boolean;")
        print(f"    green_{t}_right : boolean;")

    for s in range(S):
        print(f"    default_to_{s} : 0..{S-1};")
        print(f"    default_green_left_{s} : boolean;")
        print(f"    default_green_right_{s} : boolean;")
        
    print("ASSIGN")
    
    for t in range(T):
        print(f"    next(from_{t}) := from_{t};")
        print(f"    next(to_{t}) := to_{t};")
        print(f"    next(button_{t}_left) := button_{t}_left;")
        print(f"    next(button_{t}_right) := button_{t}_right;")
        print(f"    next(green_{t}_left) := green_{t}_left;")
        print(f"    next(green_{t}_right) := green_{t}_right;")

    for s in range(S):
        print(f"    next(default_to_{s}) := default_to_{s};")
        print(f"    next(default_green_left_{s}) := default_green_left_{s};")
        print(f"    next(default_green_right_{s}) := default_green_right_{s};")
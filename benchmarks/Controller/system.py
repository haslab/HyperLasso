import sys
import itertools

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) != 2:
        print("Usage: python system.py <states>")
        sys.exit(1)

    S = int(sys.argv[1])  # number of states

    print("MODULE main")

    print("VAR")
    print(f"    state : 0..{S-1};")
    print(f"    button_left : boolean;")
    print(f"    button_right : boolean;")
    print(f"    green_left : boolean;")
    print(f"    green_right : boolean;")

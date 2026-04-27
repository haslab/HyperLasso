import sys

if __name__ == "__main__":
    # check command line arguments
    if len(sys.argv) != 2:
        print("Usage: python robot.py <size>")
        sys.exit(1)

    S = int(sys.argv[1])  # map size

    print("MODULE main")
    print("VAR")
    print(f"    x : 0..{S-1};")
    print(f"    y : 0..{S-1};")
    print(f"    d : 0..3;")
    print("ASSIGN")
    print("    init(x) := {" + ",".join(str(i) for i in range(0,S)) + "};")
    print(f"    next(x) := case")
    print(f"        d = 0 & x < {S-1} : x + 1;")
    print(f"        d = 2 & x > 0 : x - 1;")
    print(f"        TRUE : x;")
    print("    esac;")
    print("    init(y) := {" + ",".join(str(i) for i in range(0,S)) + "};")
    print(f"    next(y) := case")
    print(f"        d = 1 & y < {S-1} : y + 1;")
    print(f"        d = 3 & y > 0 : y - 1;")
    print(f"        TRUE : y;")
    print("    esac;")
    print(f"    init(d) := {{0, 1, 2, 3}};")
    print(f"    next(d) := {{0, 1, 2, 3}};")
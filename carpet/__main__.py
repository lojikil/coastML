import sys
from . import *
if __name__ == "__main__":
    print("main", sys.argv)
    if len(sys.argv) != 3:
        print("usage: carpet.py [command] [file]")
        print("commands:\nload - load a file, and dump the resulting coastML")
        print("python - dump python from a coastML file, without the compiler")
        print("cpython - dump python from a coastML file, with the compiler")
        print("note, the last two will be merged at some point")
        sys.exit(0)

    if sys.argv[1] == "load":
        print("loading:", sys.argv[2])
        with open(sys.argv[2]) as fh:
            src = fh.read()
            c = CoastalParser(src)
            c.load()
            print(c.parse())
    elif sys.argv[1] == "python":
        print("pythonizing:", sys.argv[2])
        with open(sys.argv[2]) as fh:
            src = fh.read()
            c = CarpetPython(src)
            c.load()
            c.generate()
    elif sys.argv[1] == "cpython":
        print("compiler + python: ", sys.argv[2])
    else:
        print("unknown command:", sys.argv[1])

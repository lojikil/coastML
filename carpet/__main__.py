import sys
from . import *
if __name__ == "__main__":
    # TODO support an output mode (and default to stringio I guess)
    # TODO support a REPL
    if len(sys.argv) != 3:
        print("usage: carpet.py [command] [file]")
        print("commands:\nload - load a file, and dump the resulting coastML")
        print("compile - like load, but run the compiler too")
        print("python - dump python from a coastML file, without the compiler")
        print("cpython - dump python from a coastML file, with the compiler")
        print("javascript - dump javascript from a coastML file")
        print("note, the last two will be merged at some point")
        sys.exit(0)

    if sys.argv[1] == "load":
        print("loading:", sys.argv[2])
        with open(sys.argv[2]) as fh:
            src = fh.read()
            c = CoastalParser(src)
            c.load()
            for p in c.parse():
                print(p.to_coast())
    elif sys.argv[1] == "compile":
        print("# compiling:", sys.argv[2])
        with open(sys.argv[2]) as fh:
            src = fh.read()
            c = Compiler(src)
            for p in c.compile():
                print(p.to_coast())
    elif sys.argv[1] == "python":
        print("# pythonizing:", sys.argv[2])
        with open(sys.argv[2]) as fh:
            src = fh.read()
            c = CarpetPython(src, run_compile=False)
            c.load()
            c.generate()
    elif sys.argv[1] == "cpython":
        print("# pythonizing:", sys.argv[2])
        with open(sys.argv[2]) as fh:
            src = fh.read()
            c = CarpetPython(src, run_compile=True)
            c.load()
            c.generate()
    elif sys.argv[1] == "javascript":
        print("// javascripting:", sys.argv[2])
        with open(sys.argv[2]) as fh:
            src = fh.read()
            c = CarpetJavaScript(src)
            c.load()
            c.generate()
    else:
        print("unknown command:", sys.argv[1])

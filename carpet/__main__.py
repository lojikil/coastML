import sys
from . import *
if __name__ == "__main__":
    # TODO support an output mode (and default to stringio I guess)
    # TODO support a REPL
    if len(sys.argv) != 3:
        print("usage: carpet.py [command] [file]")
        print("commands:\nload - load a file, and dump the resulting coastML")
        print("compile - like load, but run the compiler too (useful to see what transforms are applied)")
        print("python - dump python from a coastML file")
        print("subpython - dump python from a coastML file, via the subcompiler")
        print("nopython - dump python from a coastML file, without running the compiler")
        print("javascript - dump javascript from a coastML file")
        print("pynterp - use the Python compiler as an interpreter")
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
            c = CarpetPython(src, run_compile=True)
            c.load()
            c.generate()
    elif sys.argv[1] == "subpython":
        print("# pythoning via subcompiler:", sys.argv[2])
        with open(sys.argv[2]) as fh:
            src = fh.read()
            c = Compiler(src)
            asts = c.compile_by_subpass()
            cpy = CarpetPython("", run_compile=False)
            cpy.asts = asts
            cpy.src = src
            cpy.generate()
    elif sys.argv[1] == "nopython":
        print("# pythonizing:", sys.argv[2])
        with open(sys.argv[2]) as fh:
            src = fh.read()
            c = CarpetPython(src, run_compile=False)
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

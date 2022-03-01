from carpet import *

a0 = CoastAssignAST(CoastIdentAST("Ident", "a"), CoastLiteralAST("Int", "10"))
print(a0.to_coast())
f0 = CoastFNAST([CoastIdentAST("Ident", "x")], CoastBlockAST([CoastOpCallAST(CoastIdentAST("Ident","+"), [CoastIdentAST("Ident", "x"), CoastIdentAST("Ident", "x")])]))
b0 = CoastIdentAST("Ident", "foo")
a1 = CoastAssignAST(b0, f0)
print(a1.to_coast())

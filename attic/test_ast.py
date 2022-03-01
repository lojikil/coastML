from carpet import *

a0 = CoastAssignAST(CoastIdentAST("Ident", "a"), CoastLiteralAST("Int", "10"))
print(a0.to_coast())

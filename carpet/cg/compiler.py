#@(#) this is the basic compiler, and should take many of the
#@(#) lessons and styles I've seen in the Python code generator
#@(#) and apply them more generically. The impetus for finally
#@(#) writing this was the fact that I *really* needed an
#@(#) eval-apply loop, because I was starting to worry about
#@(#) what sorts of forms I was seeing when calling pervasive
#@(#) functions, which is obviously wrong
#@(#)
#@(#) you can see the reasoning here in carpet/cg/python.py line
#@(#) 320 (or wherever `array-iter-while` lives in some future
#@(#) version)

class Compiler:
    # so, basic goals here:
    #
    # . abstract away some of the AST transforms in Python code generator
    # . check if all functions are defined (at least in terms of externs)
    # . provide the structure of where a type checker would fit in
    # . do the eval-apply loop so that the codegen doesn't need to
    #
    # I'll need to start lifting AST transforms from Python here
    pass

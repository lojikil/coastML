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

from ..parse import *


class CoastalCompilerError(Exception):
    # need to change how we store line
    # numbers; we attach them to lexemes,
    # but not ASTs, which we should start
    # doing
    def __init__(self, message, line):
        self.message = message
        self.line = line


class Compiler:
    # so, basic goals here:
    #
    # . abstract away some of the AST transforms in Python code generator
    # . check if all functions are defined (at least in terms of externs)
    # . provide the structure of where a type checker would fit in
    # . do the eval-apply loop so that the codegen doesn't need to
    #
    # I'll need to start lifting AST transforms from Python here

    def __init__(self, src, **options):
        self.src = src
        self.options = options
        self.asts = []
        self.declarations = {}
        self.variables = []
        self.functions = {}
        self.types = {}
        self.constructors = {}
        self.modules = {}
        self.import_path = []
        self.res_ctr = 0

    @staticmethod
    def from_asts(src, asts, **options):
        # create a new Compiler instance, but
        # preload the ASTs from another source,
        # such as a code gen unit
        c = Compiler(src, **options)
        c.asts = asts
        return c

    def compile(self):
        # so we need to undertake a few tasks here:
        #
        # . handle any imports
        # . check that all constructors are defined
        # . check that all functions/variables are defined
        # . check if a function is tail-recursive (for self-TCO only at first)
        # . lift all `case` and (if desired) lambdas
        # ** note we can just use the functions from `CarpetPython` here
        # . rewrite certain things to low-level pervasives, like `alien-module-call`
        # . start to infer types and the like here
        # ** in a similar vein, this is where Hoare Logic & Refinement Types would get rewritten
        # ** basically, my thought is that they should just end up as either compile time checked or runtime code
        # ** can be options to force one or the other really...
        if self.asts == []:
            parser = CoastalParser(self.src)
            parser.load()
            self.asts = parser.parse()

        new_asts = []
        for ast in self.asts:
            if type(ast) == CoastTypeDefAST:
                # we need to:
                #
                # . slice up all constructors and their arity
                # . add all the types to the top level
                new_asts += [ast]

                # ok, so constructors is actually broken down
                # as tuples of (CoastIdentAST(name), CoastLiteralList(types))
                # so here, we need to deconstruct the pairing there and
                # store the arity of the constructor. In theory we could
                # store the parameters in the dict as well...
                for ctor, ctorp in ast.constructors:
                    ctorn = "{0}.{1}".format(ast.typename, ctor)
                    if type(ctorp) == CoastLiteralAST:
                        self.constructors[ctorn] = len(ctorp.litvalue)
                        self.functions[ctorn] = len(ctorp.litvalue)
                    elif type(ctorp) == list:
                        self.constructors[ctorn] = len(ctorp)
                        self.functions[ctorn] = len(ctorp)
                    else:
                        self.constructors[ctorn] = 0
                        self.functions[ctorn] = 0
            elif type(ast) == CoastDeclareAST:
                # XXX this supports declarations in the compiler, so we
                # can use this for checking if a variable is known to
                # us. *however*, we still need to track type state here
                # and note that every declare variable is uninitialized.
                #
                # the alternative is that we set each declared variable to
                # the bottom (or default value) of that type; however, that
                # would mean mutation in languages such as OCaml would require
                # tracking. As a note, we may not want to invert `case`
                # assign statements for functional languages (or languages that
                # support expression returns really; Algol68, for example)
                self.declarations[ast.name] = ast.ntype
                new_asts += [ast]
            elif type(ast) == CoastAssignAST:
                # split: add functions to the function pile and add definitions just to the list
                if self.is_callable(ast.value):
                    # ok, here we have a function (an `fn`, a `fc` or a `gn`)
                    # that we want to record as a callable
                    new_asts += [ast]
                    # ok, and so we want to store the name and the arity of
                    # functions; we probably also should store type information
                    # but for now we can just store arity, and the typing pass
                    # can do a lookup
                    self.functions[ast.name.identvalue] = len(ast.value.parameters)
                    # really, we should `sub_compile` here, but for now
                    # I just want to get functions checked at the top level
                    #
                    # XXX we also need to check if it's self-TCO as well...
                    if self.is_self_tail_call(ast.name, ast.value):
                        ast.value.self_tail_call = True
                    #self.sub_compile(ast.value)
                elif type(ast.value) == CoastCaseAST:
                    # we need to invert `case` forms
                    # NOTE this brings up a good point:
                    # do blocks scope? they should, but we're
                    # abusing the fact that currently they do
                    # not. Really this should generate a:
                    #
                    # . current-level declare
                    # . a `set!` form in the inversion
                    #
                    # this style was ok when it was just python
                    # but now that we're in the real compiler
                    # that is used for a range of compilers, we need
                    # to fix this
                    #
                    # TODO: fix the above
                    self.is_valid_case_exn(ast.value)
                    new_asts += [self.invert_case(ast)]
                    self.variables += ast.name.identvalue
                else:
                    # XXX we should check for a function call here and
                    # lift `case` and the like
                    # here, we have a simple value assignment
                    new_asts += [ast]
                    self.variables += ast.name.identvalue
            elif type(ast) == CoastCaseAST:
                # here we have to:
                #
                # . [x] check if the condition is a call that requires a lift
                # . [x] check if all cases make syntactic sense
                self.is_valid_case_exn(ast)
                if type(ast.initial_condition) == CoastFNCallAST or \
                   type(ast.initial_condition) == CoastOpCallAST:
                    (sub_ic, sub_ic_newast) = self.lift_call_with_case(ast.initial_condition)
                    # ok, so we either have:
                    #
                    # . a list of new bindings for the condition OR
                    # . an empty list
                    #
                    # in the case of the former, we need to build a new
                    # `case` form and return the prepended list of forms
                    nc = CoastCaseAST(sub_ic_newast, ast.conditions)
                    new_asts += sub_ic
                    new_asts += [nc]
                else:
                    new_asts += [ast]
            elif type(ast) == CoastOpCallAST or type(ast) == CoastFNCallAST:
                (lifted, newcall) = self.lift_call_with_case(ast)
                new_asts += lifted
                new_asts += [newcall]
                # NOTE would be really nice to make recommendations here
                # for example, if you try to call "ripnt", we could recommend
                # "print"; need to port my Levenshtein code from Reason...
                if type(ast) == CoastFNCallAST:
                    # we have a function; check that it's a function we know
                    # about, such as a basis function or one that the user has
                    # defined
                    if not self.is_basis_fn(ast.fn) and ast.fn.identvalue not in self.functions:
                        raise CoastalCompilerError("undefined function: \"{0}\"".format(ast.fn.identvalue), 0)
                else:
                    # we have an operator, check if it's one we know about
                    pass

                # XXX and here we need to check all variables to see if we
                # know about those as well...
            else:
                # here, we need to iterate over all the members anyway, and make sure they're
                # all defined...
                pass
        self.asts = new_asts
        return new_asts

    def sub_compile(self, fn):
        # iterate over the forms in `fn` to make sure that each is
        # lifted as needed and defined
        return fn

    def is_callable(self, fn):
        return type(fn) == CoastFNAST or type(fn) == CoastGNAST or type(fn) == CoastFCAST

    # TODO need to split these out a bit more:
    #
    # . unary pervasive operators
    # . binary pervasive operators
    # . arity of all other pervasives
    # . types (in a coastLine file?)
    def is_basis_fn(self, fn):
        # is this one of the basis functions we know
        # how to optimize away?
        basislib = ["array-length", "array-get", "array-set!", "array-make",
                    "array-init", "array-make-matrix", "array-append",
                    "array-append!", "array-concat", "array-concat!",
                    "array-sub", "array-copy", "array-fill!", "array-blit!",
                    "array->list", "list->array", "array-iter", "array-iter-while",
                    "array-map", "array-iter-index", "array-map-index", "array-iter-until",
                    "array-foldl", "array-foldr", "array-sort", "array-sort!",
                    "array-stable-sort", "array-fast-sort", "string-length",
                    "string-get", "string-make", "string-init", "string-split",
                    "string-append", "string-join", "string-contains",
                    "string-concat", "string-copy", "string->array",
                    "string-iter", "string-map", "string-iter-index", "string-iter-while",
                    "string-map-index", "string-foldl", "string-foldr", "string-iter-until",
                    "string-sort", "compare", "char-code", "char-chr",
                    "char-escaped", "char-lowercase", "char-uppercase",
                    "char-compare", "random-int", "random-float", "random-int-range",
                    "random-bool", "random-choice"]
        return fn.identvalue in basislib

    def is_accessor(self, fn):
        # checks if we have a call to an accessor;
        # the compiler should take care of if this
        # accessor is _meaningful_ or not, so here we
        # just need to know what to dispatch to
        if fn.identvalue[0] == '_':
            return True
        return False

    def is_accessor_variable(self, var):
        # for case statements and the like, when we
        # wish to access a member of the current
        # ADT without referring to the original
        # itself
        if var.identvalue[0] == '$':
            return True
        return False

    def is_unit(self, call):
        # is this something that returns unit
        # (aka, is this a procedure) and potentially
        # is rewritten to a lower-level form that
        # we shouldn't necessarily rely on being a
        # function call
        if type(call) == CoastFNCallAST and \
            self.is_basis_fn(call.fn) and \
            ("iter" in call.fn.identvalue or "!" in call.fn.identvalue):
            return True
        return False

    def is_valid_case_exn(self, ast, env=None):
        # actually, we can do *all* checks here, just make the `if`
        # below check if it's a function _first_, and then we can
        # do whatever there

        if type(ast.initial_condition) is CoastFNCallAST and \
           not self.is_basis_fn(ast.initial_condition.fn) and \
           ast.initial_condition.fn.identvalue not in self.functions:
            # we need something to check a env object when passed in...
            #if env is not None and ast.initial_condition.fn.indentvalue no in env["functions"]:
            #    raise CoastalCompilerError("undefined function: \"{0}\"".format(ast.initial_condition.fn.identvalue), 0)
            #else:
            raise CoastalCompilerError("undefined function: \"{0}\"".format(ast.initial_condition.fn.identvalue), 0)

        for cnd in ast.conditions:
            if type(cnd[0]) == CoastFNCallAST and \
               not self.is_basis_fn(cnd[0].fn) and \
               cnd[0].fn.identvalue not in self.functions:
                raise CoastalCompilerError("undefined function: \"{0}\"".format(cnd[0].fn.identvalue), 0)

    def is_self_tail_call(self, name, call):
        # ok, so we need to walk down the spine of a function
        # and check if there's even a _single_ tail call therein.
        # then we can just mark this as true and leave the actual
        # work to the individual code generators
        if self.is_callable(call):
            return self.is_self_tail_call(name, call.body)
        elif type(call) == CoastBlockAST:
            # walk call.progn and check the last
            # member
            return self.is_self_tail_call(name, call.progn[-1])
        elif type(call) == CoastCaseAST:
            # here, we just have to walk each case and
            # check if the then-arm contains a call
            for c in call.conditions:
                if self.is_self_tail_call(name, c[1]):
                    return True
        elif type(call) == CoastFNCallAST:
            # also need to check that it even IS an
            # ident
            if type(call.fn) == CoastIdentAST:
                return call.fn.identvalue == name.identvalue
        else:
            # we _probably_ won't have another form here,
            # but who knows.
            pass
        return False

    def mung_ident(self, ident):
        src = ""
        # XXX We need to decide if we handle modules here
        # or in the actual python code...
        if type(ident) is CoastIdentAST:
            src = ident.identvalue
        else:
            src = ident
        return src.replace("->", "2").replace("-", "_").replace("?", "_p")

    def invert_case(self, ast, depth=0, tail=False):
        # ok, here we just need to thread the assigned variable into
        # the last form of each block, and then just call `generate_case`
        varname = ast.name.identvalue
        varval = ast.name
        case = ast.value
        newcase = CoastCaseAST(case.initial_condition, case.conditions)
        for cndidx in range(0, len(case.conditions)):
            cnd = case.conditions[cndidx]
            test = cnd[0]
            # make a copy of the conditions, then modify them
            then = cnd[1].progn[:]
            last = CoastAssignAST(varval, then[-1])
            then[-1] = last
            newcase.conditions[cndidx] = [test, CoastBlockAST(then)]

        return newcase

    def lift_call_with_case(self, ast):
        # similar to the above, we actually:
        #
        # . freshsym a `case` form in a function call
        # . call the case threader above
        # . then call our normal function call here
        #
        # we actually need to be able to lift a several case forms
        # here, and correctly, so that also could be interesting to
        # register...
        lifted = []
        if type(ast) == CoastFNCallAST:
            newast = CoastFNCallAST(ast.fn, [])
        else:
            newast = CoastOpCallAST(ast.op, [])
        # we actually need to check if we're generating a `op` or a
        # `fn` call here as well; both have `.data` as a member, but
        # to setup the actual call we need to know
        for arg in ast.data:
            if type(arg) == CoastCaseAST:
                # ok, we need to lift this here, and rewrite the access to the
                # lifted var
                varname = self.generate_freshsym_string("cval")
                varast = CoastIdentAST(TokenIdent, varname)
                newassign = CoastAssignAST(varast, arg)
                lifted.append(newassign)
                newast.data.append(varast)
            elif type(arg) == CoastFNCallAST or type(arg) == CoastOpCallAST:
                # so here, we recurse into another `lift_call_wtih_case` call,
                # and then merge the results. For most of these, nothing likely
                # happens
                (sublifted, subnewast) = self.lift_call_with_case(arg)
                lifted += sublifted
                newast.data.append(subnewast)
            elif self.is_callable(arg):
                # lift functions in params...
                # also, it makes me wonder if these should ALWAYS be lifted;
                # small lambda's are more common in some languages, like JavaScript,
                # but impossible or disuaded in others (like Python)
                varname = self.generate_freshsym_string("fn")
                varast = CoastIdentAST(TokenIdent, varname)
                newassign = CoastAssignAST(varast, arg)
                lifted.append(newassign)
                newast.data.append(varast)
            else:
                # here, we just need to copy the argument to the new AST
                newast.data.append(arg)
        return (lifted, newast)

    def generate_fn_inverted_case(self, ast, depth=0, tail=False):
        # actually lift arguments, if any, into `case` forms, and
        # then process through each of them
        (lifted, newast) = self.lift_call_with_case(ast)

        for l in lifted:
            self.generate_inverted_case(l, depth=depth, tail=tail)

        self.generate_call(newast, depth=depth, tail=tail)

    def generate_shadows_self_tail_call(self, ast):
        # ok, so we know we have a self-TC lambda here,
        # but we don't _really_ want to insert other pseudo
        # instructions, and we don't want to rely on the
        # code generators knowing what to do, other than
        # wrapping the body of this lambda in a `while` or
        # other loop. So this method will:
        #
        # . walk the spine of the `block`
        # . generate the initial shadow params
        # . rewrite any calls to `self` as a shadow-swap of parameters
        # . remove the call
        # . return the new AST

        # this should hold the current idx and AST
        work_queue = []
        idx = 0
        tast = None

        ret = None

        if type(ast) == CoastFNAST:
            ret = CoastFNAST(None, None)
        elif type(ast) == CoastGNAST:
            ret = CoastGNAST(None, None, None)
        elif type(ast) == CoastFCAST:
            ret = CoastFCAST(None, None)


        while len(work_queue) > 0:
            if self.is_callable(call):
                return self.is_self_tail_call(name, call.body)
            elif type(call) == CoastBlockAST:
                # walk call.progn and check the last
                # member
                return self.is_self_tail_call(name, call.progn[-1])
            elif type(call) == CoastCaseAST:
                # here, we just have to walk each case and
                # check if the then-arm contains a call
                for c in call.conditions:
                    if self.is_self_tail_call(name, c[1]):
                        return True
            elif type(call) == CoastFNCallAST:
                # also need to check that it even IS an
                # ident
                if type(call.fn) == CoastIdentAST:
                    return call.fn.identvalue == name.identvalue
            else:
                # we _probably_ won't have another form here,
                # but who knows.
                pass
            return False

    def generate_freshsym_string(self, basename=None):
        n = "res"

        if basename is not None:
            n = basename

        n = "{0}{1}".format(n, self.res_ctr)
        self.res_ctr += 1
        return n

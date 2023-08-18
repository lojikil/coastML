from ..parse import *
from .compiler import Compiler

# The actual coastML -> Python compiler
# named after the "coastal carpet python"
class CarpetPython:
    def __init__(self, src, indent="    ", fh=None, run_compile=True):
        self.fns = {}
        self.vals = {}
        self.src = src
        self.asts = []
        self.indent = indent
        self.res_ctr = 0
        self.compile = run_compile
        if fh is None:
            self.fh = io.StringIO("")
        else:
            self.fh = fh

    def load(self):
        self.fns = {}
        self.vals = {}
        self.asts = []
        self.res_ctr = 0
        parser = CoastalParser(self.src)
        parser.load()
        self.asts = parser.parse()
        if self.compile:
            comp = Compiler.from_asts(self.src, self.asts)
            self.asts = comp.compile()

        for ast in self.asts:
            if type(ast) == CoastAssignAST and \
               (type(ast.value) == CoastFNAST or \
                type(ast.value) == CoastFCAST or \
                type(ast.value) == CoastGNAST):
                n = ast.name.identvalue
                self.fns[n] = ast.value
            elif type(ast) == CoastAssignAST:
                n = ast.name.identvalue
                self.vals[n] = ast.value

    def is_callable(self, fn):
        return type(fn) == CoastFNAST or type(fn) == CoastGNAST or type(fn) == CoastFCAST

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
                    "char-compare", "foreign-object-type", "foreign-call",
                    "foreign-module-call", "foreign-class-call", "foreign-accessor"]
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

    def mung_ident(self, ident):
        src = ""
        # XXX We need to decide if we handle modules here
        # or in the actual python code...
        if type(ident) is CoastIdentAST:
            src = ident.identvalue
        else:
            src = ident
        return src.replace("->", "2").replace("-", "_").replace("?", "_p")

    def generate_indent(self, cnt):
        for i in range(0, cnt):
            print(self.indent, end='')

    # we have to lift lambdas most places in Python
    # because our lambdas are much more expressive
    # than the ones that Python allows; probably need
    # to run a quick check on them...
    def generate_fn(self, fn, depth=0, tail=False):
        n = self.mung_ident(fn.name.identvalue)
        v = fn.value
        params = ", ".join([self.mung_ident(x.to_coast())
                            for x in v.parameters])
        self.generate_indent(depth)
        print("def {0}({1}):".format(n, params))
        if v.self_tail_call:
            self.generate_self_tail_call(n, v, depth=depth+1)
        else:
            self.generate_block(v.body, tail=tail, depth=depth+1)

    def generate_assignment(self, ast, depth=0):
        n = ast.name.identvalue
        v = ast.value
        if type(v) == CoastFNCallAST or type(v) == CoastOpCallAST:
            # FIXME this isn't identing correctly...
            (lifted, newast) = self.lift_call_with_case(v)
            for l in lifted:
                self.generate_inverted_case(l, depth=depth, tail=False)
            if len(lifted) != 0:
                self.generate_indent(depth)
            print("{0} = ".format(n), end='')
            self.generate_call(newast, depth=0)
        else:
            print("{0} = ".format(n), end='')
            self.generate_dispatch(v, depth=depth+1)

    def generate_literal(self, v, depth=0):
        print(v.value, end='')

    def generate_block(self, block, depth=0, tail=False):
        # we need to track if this or the call
        # is in the tail position, and return
        # from there, or really any form...
        l = len(block.progn)
        o = 0
        for b in block.progn:
            if tail and o == (l - 1):
                # XXX this is a bit of a hack, but works
                # for now...
                if type(b) == CoastFNCallAST and \
                   type(b.fn) == CoastIdentAST and \
                   b.fn.identvalue == "%shadow-recurse":
                    pass
                else:
                    self.generate_indent(depth)
                    self.generate_dispatch(b, depth=depth, tail=True)
            else:
                self.generate_indent(depth)
                self.generate_dispatch(b, depth=depth)
            print("")
            o += 1

    def generate_self_tail_call(self, name, val, depth=0):
        # generally here what I've done previously is to
        # annotate where we shadow parameters and then
        # generate the function normally...
        #
        # here, we need to actually walk the spine
        # ourselves, and shadow rather than return...
        # it might be easier to add a pseudo-AST like
        # `self-tail-call` with the parameters...
        #
        # OH... *or* we could have the compiler rewrite
        # those for us... then the code generator just
        # needs to insert a `while` or the like...
        # something like `lift_tail_calls` in the
        # compiler, as well as an option to determine
        # *how* we do that (if at all). Then here, all
        # we need to do is generate a `while` (or w/e)
        # and then use normal functions to do the rest,
        # since in theory the compiler has taken care
        # of the various rewrites for us

        self.generate_indent(depth)

        # XXX it would be nice to remove single-arm `case`
        # forms here; for example:
        #
        # [source]
        # ----
        # case n
        # | (n > 0) { ... }
        # | _ { ... }
        # esac
        # ----
        #
        # this could have some loop motion:
        #
        # [source]
        # ----
        # while (n > 0) {
        #     # do the `n > 0` then here
        # }
        # # do the `_` then body here
        # ----
        #
        # this would simplify many of the checks and make
        # the resulting code look quite nice

        print('while True:')

        # we need to actually just add `return` for all returns
        # and non-self-tail-calls, and elide anything else
        self.generate_block(val.body, tail=True, depth=depth+1)

    def generate_type(self, t, depth=0, tail=False):
        # this is going to be interesting; basically, we
        # need to generate the entire class hierarchy of
        # a type, starting with the basetype (which is the
        # name) and iterating through to generating each
        # class' `__init__` and so on.
        #
        # NOTE also, eventually we'll have named fields
        # which we'll have to take care of as well
        #
        # NOTE we *also* now need to add constructors to
        # `case` forms
        base = self.mung_ident(t.typename)
        print("class {0}:".format(base))
        self.generate_indent(depth + 1)
        print("pass\n")
        for ctor in t.constructors:
            print("@dataclass\nclass {0}_{1}({0}):".format(base, self.mung_ident(str(ctor[0]))))
            ctorp = ctor[1]
            if type(ctorp) is CoastLiteralAST and len(ctorp.litvalue) > 0:
                m = 0
                for p in ctorp.litvalue:
                    self.generate_indent(depth + 1)
                    print("m_{0} : ".format(m), end='')
                    m += 1
                    self.generate_cardinal_type(p, depth=0)
                    print("")

            elif type(ctorp) is list and len(ctorp) > 0:
                # XXX this is dead code I believe, but I need to
                # test it more to be sure
                # also, this sort of thing is what got us in trouble
                # in carML, so let's really test this and make sure
                # we never need it
                # TODO remove this
                print("# here in list for", ctorn)
                params = " ".join([x.to_coast() for x in ctorp])
                ctors.append("| {0} is [{1}]".format(ctorn.to_coast(),
                                                     params))
            else:
                self.generate_indent(depth + 1)
                print("pass")
            print("")

    def generate_cardinal_type(self, t, depth=0, tail=False):
        # for right now, we're not attempting to generate
        # parameterized types in Python3, but that should
        # change
        if t.basetype == "string":
            print("str", end='')
        elif t.basetype == "char":
            print("str", end='')
        elif t.basetype == "num":
            # NOTE: must remember to include `from numbers import Number`
            print("Number", end='')
        elif t.basetype == "array":
            print("list", end='')
        elif t.basetype == "foreign":
            self.generate_cardinal_type(t.typeparameters.litvalue[0])
        elif t.basetype == 'function':
            # NOTE here we actually should iterate
            # over the other types and print them
            # have to actually get this correct:
            # https://docs.python.org/3/library/typing.html#annotating-callable-objects
            # so the syntax should be `Callable[[int], str]`
            # for a function that accepts one integer
            # parameter and returns a string.
            print("Callable[", end='')
            params = t.typeparameters.litvalue[0:-1]
            retprm = t.typeparameters.litvalue[-1]
            if len(params) != 0:
                lp = len(params)
                idx = 0
                print("[", end="")
                for subt in params:
                    self.generate_cardinal_type(subt)
                    if idx <= (lp - 2):
                        print(", ", end="")
                    idx += 1
                print("], ", end="")
            if retprm != None:
                self.generate_cardinal_type(retprm)
            print("]", end='')
        else:
            print(t.basetype, end='')

    def generate_accessor_string(self, resv:Union[str, None], accessor:Union[int, str]) -> str:
        if type(accessor) is int or (type(accessor) is str and accessor.isnumeric()):
            finalaccess = "m_{0}".format(accessor)
        else:
            finalaccess = self.mung_ident(accessor)

        if resv is None:
            return ".{0}".format(finalaccess)

        return "{0}.{1}".format(resv, finalaccess)

    def generate_array(self, ast, depth=0):
        print('[', end='')
        l = len(ast.litvalue)
        for i in range(0, l):
            self.generate_dispatch(ast.litvalue[i], depth=0)
            if i < (l - 1):
                print(', ', end='')
        print(']', end='')

    def generate_basis(self, call:CoastFNCallAST, depth=0):
        # many of these will require some minimal amount of custom
        # code to support...
        basisname = call.fn.identvalue
        if basisname == "array-length" or basisname == "string-length":
            print("len(", end='')
            self.generate_dispatch(call.data[0], depth=0)
            print(")", end='')
        elif basisname == "array-get":
            self.generate_dispatch(call.data[0], depth=0)
            print("[", end='')
            self.generate_dispatch(call.data[1], depth=0)
            print("]", end='')
        elif basisname == "array-set!":
            self.generate_dispatch(call.data[0], depth=0)
            print("[", end='')
            self.generate_dispatch(call.data[1], depth=0)
            print("] = ", end='')
            self.generate_dispatch(call.data[2], depth=0)
        elif basisname == "array-make":
            print("[", end='')
            self.generate_dispatch(call.data[1], depth=0)
            print(" for _ in range(0, ", end='')
            self.generate_dispatch(call.data[0], depth=0)
            print(")]", end='')
        elif basisname == "array-init":
            print("[", end='')
            # NOTE we actually have to bind the value here and
            # use it for each call to the function in the line
            # above... the issue here is that python will actually
            # clobber any variable, so we need a freshsym here
            sym = self.generate_freshsym_string('x')
            self.generate_dispatch(call.data[1], depth=0)
            print("({0}) for {0} in range(0, ".format(sym), end='')
            self.generate_dispatch(call.data[0], depth=0)
            print(")]", end='')
        elif basisname == "array-make-matrix":
            print('[[', end='')
            self.generate_dispatch(call.data[2], depth=0)
            print(' for _ in range(0, ', end='')
            self.generate_dispatch(call.data[0], depth=0)
            print(')] for _ in range(0, ', end='')
            self.generate_dispatch(call.data[1], depth=0)
            print(')]', end='')
        elif basisname == "array-map":
            # literally just a list comprehension
            # map-index is the same, but with a
            # range and passing the value of current
            # position and value to the function
            # `iter` is the same but just in a for loop
            print('[', end='')
            sym = self.generate_freshsym_string('x')
            self.generate_dispatch(call.data[0], depth=0)
            print("({0}) for {0} in ".format(sym), end='')
            self.generate_dispatch(call.data[1], depth=0)
            print("]", end='')
        elif basisname == "array-map-index":
            # literally just a list comprehension
            # map-index is the same, but with a
            # range and passing the value of current
            # position and value to the function
            # `iter` is the same but just in a for loop
            print('[', end='')
            idx = self.generate_freshsym_string('idx')
            self.generate_dispatch(call.data[0], depth=0)
            print("({0}, ".format(idx), end='')
            self.generate_dispatch(call.data[1], depth=0)
            print("[{0}]) for {0} in range(0, len(".format(idx), end='')
            self.generate_dispatch(call.data[1], depth=0)
            print("))]", end='')
        elif basisname == "array-iter" or basisname == "string-iter":
            # NOTE this is an interesting potential optimization for the
            # compiler: use alpha conversion to rename the variables to
            # fresh sysms and remove the function call entirely in favor
            # of a loop...
            sym = self.generate_freshsym_string('x')
            print('for {0} in '.format(sym), end='')
            self.generate_dispatch(call.data[1], depth=0)
            print(':')
            self.generate_indent(depth+1)
            self.generate_dispatch(call.data[0], depth=0)
            print('({0})'.format(sym))
        elif basisname == "array-iter-while" or \
             basisname == "array-iter-until" or \
             basisname == "string-iter-while" or \
             basisname == "string-iter-until":
            # NOTE wow an actual location wherein a `do...while` loop would
            # actually be the more efficient solution! here, we need to:
            #
            # . lift the call (in case it has any functions or `case` forms)
            # . generate the lifted members *before* the `while`
            # . generate the body of the `iter` within the `while`
            # . generate the lifted members again (so as to re-eval the `case`)
            #
            # which is probably a bit more than we expected...
            # actually, thinking about that, how would that work for map?
            # Thinking further, I don't believe we actually need that second
            # case generation, since it actually just gets eval'd the one time...

            # thinking about these two... we need an eval-apply loop really
            #
            # thinking even FURTHER about it, these should in the general case
            # just be named idents, so these calls are likely a waste; it's not
            # impossoble that someone could pass in a call, but it's not the common
            # case. Additionally, what I'll do for now is actually handle things here
            # and we can double up in the compiler. Lastly, since we are using
            # `lift_call_with_case` for the general portion, we actually *do* have a
            # eval-apply loop going on, of a sort
            # XXX fix these, we're attemtping to operate on idents here...
            if type(call.data[0]) == CoastFNCallAST or type(call.data[0]) == CoastOpCallAST:
                (plifted, pnewast) = self.lift_call_with_case(call.data[0]) # predicate lift
            else:
                (plifted, pnewast) = ([], call.data[0])

            # body lift
            if type(call.data[1]) == CoastFNCallAST or type(call.data[1]) == CoastOpCallAST:
                (blifted, bnewast) = self.lift_call_with_case(call.data[1])
            else:
                (blifted, bnewast) = ([], call.data[1])

            # array lift
            if type(call.data[2]) == CoastFNCallAST or type(call.data[2]) == CoastOpCallAST:
                (alifted, anewast) = self.lift_call_with_case(call.data[2])
            else:
                (alifted, anewast) = ([], call.data[2])

            lifts = plifted + blifted + alifted
            for lift in lifts:
                self.generate_inverted_case(lift, depth=depth, tail=False)

            # here, we need to tell if we're doing a while or a while not and
            # then go from there

            # we need to actually check what we have here, and
            # if we have a call-able, we need to interleave that
            # call here
            # as well, we need to generate a binding for the initial
            # pass, and then assign it in the loop itself...
            res = self.generate_freshsym_string("res")
            resi = CoastIdentAST(TokenIdent, res)

            # originally, I was going for `while` forms, but I realized
            # that a `for...break` pattern is idiomatic and not terrible
            # either; it means the code generator can spend less time on
            # thinking about tracking the variables themselves
            print("for {0} in ".format(res), end='')
            self.generate_dispatch(anewast, depth=0, tail=False)
            print(":")

            if basisname.endswith("-while"):
                self.generate_indent(depth + 1)
                print("if not ", end='')
            else:
                self.generate_indent(depth + 1)
                print("if ", end='')

            if type(pnewast) == CoastFNCallAST or type(pnewast) == CoastOpCallAST:
                self.generate_call(pnewast, depth=0, tail=False)
            else:
                call_pnewast = CoastFNCallAST(pnewast, [resi])
                self.generate_call(call_pnewast, depth=0, tail=False)

            print(":")
            self.generate_indent(depth + 2)
            print("break")

            # ok, we have the general outline here, now we need
            # to generate the body
            self.generate_indent(depth + 1)
            if type(bnewast) == CoastFNCallAST or type(bnewast) == CoastOpCallAST:
                self.generate_call(bnewast, depth=0, tail=False)
            else:
                call_bnewast = CoastFNCallAST(bnewast, [resi])
                self.generate_call(call_bnewast, depth=0, tail=False)

        elif basisname == "array-iter-index" or \
             basisname == "string-iter-index":
            # also for here, we can freshsym a binding for
            # the data if it isn't a variable already...
            # for example, if it's a function call, memoize that to a
            # freshsym and then use that for all processing thereafter
            idx = self.generate_freshsym_string('idx')
            sym = self.generate_freshsym_string('x')
            print('for {0} in range(0, len('.format(idx), end='')
            self.generate_dispatch(call.data[1], depth=0)
            print(')):')
            self.generate_indent(depth+1)
            print('{0} = '.format(sym), end='')
            self.generate_dispatch(call.data[1], depth=0)
            print('[{0}]'.format(idx))
            self.generate_indent(depth+1)
            self.generate_dispatch(call.data[0], depth=0)
            print('({0}, {1})'.format(idx, sym))
        elif basisname == "array-append!":
            # NOTE: this sort of thing is *perfect* for the
            # `alien-class-module` type I was thinking about
            self.generate_dispatch(call.data[0], depth=0)
            print('.append(', end='')
            self.generate_dispatch(call.data[1], depth=0)
            print(')', end='')
        elif basisname == "array-append" or basisname == "string-append":
            self.generate_dispatch(call.data[0], depth=0)
            print(' + ', end='')
            self.generate_dispatch(call.data[1], depth=0)
        elif basisname == "array-blit!":
            pass
        elif basisname == "array-concat!":
            pass
        elif basisname == "array-concat":
            print('functools.reduce(lambda x, y: x + y,', end='')
            self.generate_dispatch(call.data[0], depth=0)
            print(')', end='')
        elif basisname == "array-copy":
            self.generate_dispatch(call.data[0], depth=0)
            print('.copy()', end='')
        elif basisname == "array-sort!":
            self.generate_dispatch(call.data[0], depth=0)
            print(".sort()")
        elif basisname == "array-sub":
            # XXX: this either needs to be checked to be
            # within range by the compiler, or we really
            # need to rename this `array-sub-unsafe` or
            # `array-sub-exn`
            self.generate_dispatch(call.data[0], depth=0)
            print('[', end='')
            self.generate_dispatch(call.data[1], depth=0)
            print(':', end='')
            self.generate_dispatch(call.data[2], depth=0)
            print(']', end='')
        elif basisname == "string-get":
            self.generate_dispatch(call.data[0], depth=0)
            print("[", end='')
            self.generate_dispatch(call.data[1], depth=0)
            print("]", end='')
        elif basisname == "string-split":
            self.generate_dispatch(call.data[0], depth=0)
            print(".split(", end='')
            self.generate_dispatch(call.data[1], depth=0)
            print(")", end='')
        elif basisname == "string-contains":
            self.generate_dispatch(call.data[1], depth=0)
            print(" in ", end='')
            self.generate_dispatch(call.data[0], depth=0)
        elif basisname == "string-map":
            # so I was thinking there are two ways of doing this:
            # `''.join(list(map(func, str)))`, which would basically
            # be p simple, or the same but with a list comprehension.
            # either should be relatively straight forward I think...
            print("''.join(list(map(", end='')
            self.generate_dispatch(call.data[0], depth=0)
            print(", ", end='')
            self.generate_dispatch(call.data[1], depth=0)
            print(")))", end='')
        elif basisname == "string-map-index":
            # now *this* get's a little more interesting...
            # we could map over zip here...
            pass
        elif basisname == "foreign-object-type":
            print("type(", end='')
            self.generate_dispatch(call.data[0], depth=0)
            print(").__name__", end='')
        elif basisname == "foreign-call":
            print(call.data[0].litvalue[1:-1] + "(", end='')
            l = len(call.data)
            for i in range(1, len(call.data)):
                self.generate_dispatch(call.data[i], depth=0)
                if i < (l - 1):
                    print(", ", end='')
            print(")", end='')
        elif basisname == "foreign-accessor":
            self.generate_dispatch(call.data[1], depth=0)
            print(".{0}".format(call.data[0].litvalue[1:-1]))
        else:
            print("willimplementlater()", end='')

    def generate_constructor_case(self, resv:str, test:CoastAST) -> list[tuple[str, CoastAST]]:
        # ok, so we need to:
        #
        # . iterate over a `test`
        # . generate the `type($resv) == TypeName_ConstructorName`
        # . generate any subconditions based off of accessors & guards
        # . return a set of bindings that users may want to use...
        #
        # it's not _terrible_ really, we just need to actually pay
        # attention to positions. More interesting to me is how do
        # we map an accessor to the classes we generate? I've thought about
        # having to add methods to those to help with it, but that also
        # seems needlessly expensive. We could carry around or look up
        # what constructor definitions we have, that would be easier, but
        # we need to actually add that information in somewhere... This is
        # made somewhat easier by the fact that I don't actually support
        # naming Constructor arguments currently, so we know that `_1` actually
        # needs to be rewritten to `obj.m_1`
        #
        # the other aspect to this is that I want to support really simple
        # guard clauses; like `(Option.Some (_ > 10))`, and here we would
        # positionally rewrite the `_` to `$1` and then rewrite that to the
        # correct constructor member
        clazz_name = test.fn.identvalue.replace('.', '_')
        bindings = []

        # so we keep an id here to count off the index of the constructor
        # member, and then iterate over each piece of data and rewrite the
        # names in order...

        idx = 0

        print('type({0}) == {1}'.format(resv, clazz_name), end='')

        for d in test.data:
            if type(d) == CoastIdentAST and \
               d.identvalue == "_":
                idx += 1
            elif type(d) == CoastIdentAST:
                # need to generate a binding here
                # and return it to the callee
                # this is also interesting because is this an
                # equality check (like does the value EQUAL the
                # variable's value) or is it always a binding?
                bindings.append((d, self.generate_accessor_string(resv, idx)))
                idx += 1
            elif type(d) == CoastLiteralAST:
                # generate a `$res.m_$idx == ${d.to_coast}` here
                print(' and ', end='')
                print(self.generate_accessor_string(resv, idx), "==", d.to_coast(), end='')
                idx += 1
            elif type(d) == CoastFNCallAST or type(d) == CoastOpCallAST:
                idx += 1

        return bindings

    def generate_inverted_case(self, ast, depth=0, tail=False):
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
        self.generate_case(newcase, depth=depth, tail=tail)

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

    def generate_freshsym_string(self, basename=None):
        n = "res"

        if basename is not None:
            n = basename

        n = "{0}{1}".format(n, self.res_ctr)
        self.res_ctr += 1
        return n

    def generate_case(self, case, depth=0, tail=False):
        ctr = 0
        if case.initial_condition is not None:
            if (type(case.initial_condition) is CoastFNCallAST or \
               type(case.initial_condition) is CoastOpCallAST):
                # we need to generate a holder variable here, and
                # use that in all of our test cases...
                # we also need to figure out when to bind in case
                # clauses...
                resv = "res" + str(self.res_ctr)
                self.res_ctr += 1
                # NOTE there's an interesting edge case here: if we have a `case`
                # within the function we're attempting to call here, ex:
                #
                # [source]
                # ----
                # case (some-lambda 10 case x | 11 { "eleven"} | _ { "whoops" } esac)
                # ...
                # esac
                # ----
                #
                # we technically don't handle that case here; what we should do
                # is generate the freshsym, then hand that off to `generate_fn_inverted_case`
                # then check if it's ok
                print("{0} = ".format(resv), end='')
                self.generate_call(case.initial_condition, depth=0, tail=False)
                print("")
                # NOTE we do this so as to support the
                # initial `if` form
                self.generate_indent(depth)
            else:
                resv = case.initial_condition.identvalue

            for cnd in case.conditions:
                test = cnd[0]
                then = cnd[1]
                bindings = None
                if type(test) is CoastIdentAST and test.identvalue == "_":
                    self.generate_indent(depth)
                    print("else:")
                    self.generate_block(then, depth=depth + 1, tail=tail)
                elif type(test) is CoastFNCallAST or \
                     type(test) is CoastOpCallAST:
                    # this is actually tricky, because we should be checking
                    # for bindings here, as well as for type constructors and
                    # doing destructuring bind from there... for now, we can
                    # just run things really
                    if ctr > 0:
                        # we don't need to indent for the initial `if`, because
                        # we can assume it's properly handled at the block level
                        self.generate_indent(depth)
                        print('elif ', end='')
                    else:
                        print('if ', end='')

                    # ok, so if we have a type constructor, we want
                    # to actually generate a `type(...) is ...` check
                    # and return any bindings we need to set in the
                    # then block
                    if type(test) is CoastFNCallAST and \
                       type(test.fn) is CoastIdentAST and \
                       test.fn.identtype is TokenNSADT:
                        bindings = self.generate_constructor_case(resv, test)
                    else:
                        self.generate_call(test, depth=1, tail=False)

                    print(':')

                    if bindings is not None:
                        for binding in bindings:
                            self.generate_indent(depth + 1)
                            print("{0} = {1}".format(binding[0], binding[1]))

                    ctr += 1
                    self.generate_block(then, depth=depth + 1, tail=tail)
                else:
                    if ctr > 0:
                        self.generate_indent(depth)
                        print('elif ', end='')
                    else:
                        print('if ', end='')
                    print('{0} == '.format(resv), end='')
                    self.generate_dispatch(test, depth=0, tail=False)
                    print(':')
                    self.generate_block(then, depth=depth + 1, tail=tail)
                    ctr += 1
        else:
            # we're here, so we have no initial condition, but
            # we do have a bunch of test cases we need to generate
            # this is for when `case` is acting like `cond`
            for clause in case.conditions:
                test = clause[0]
                then = clause[1]

                if type(test) is CoastIdentAST and test.identvalue == "_":
                    self.generate_indent(depth)
                    print("else:")
                elif ctr > 0:
                    self.generate_indent(depth)
                    print("elif ", end="")
                    self.generate_call(test, depth=0, tail=False)
                    print(":")
                else:
                    print("if ", end="")
                    self.generate_call(test, depth=0, tail=False)
                    print(":")

                self.generate_block(then, depth=depth, tail=tail)
                ctr += 1

    def generate_call(self, call, depth=0, tail=False):
        if tail and not self.is_unit(call):
            # we need to be able to check if the item...
            #
            # . is a tail call (and thus we rewrite to a loop)
            # . returns `unit`, and thus does not need a return
            #
            # makes me think we need to actually do the type system
            # currently to make those sorts of adjustments first...
            # a quick hack would be to just add a check for internal
            # items we know are rewritten and then ignore the rest
            # (for now), since it doesn't matter if a function call
            # returns `None` or `unit`, only that we don't generate
            # syntactically invalid Python
            print("return ", end='')

        if type(call) == CoastFNCallAST and \
           self.is_basis_fn(call.fn):
            self.generate_basis(call, depth=depth)
        elif type(call) == CoastFNCallAST and \
             self.is_accessor(call.fn):
            self.generate_dispatch(call.data[0])
            accessor = call.fn.identvalue[1:]
            print(self.generate_accessor_string(None, accessor), end='')
        elif type(call) == CoastFNCallAST and \
             call.fn.identtype == TokenNSADT:
            # We have a constructor of some type here; split the name and just generate as normal
            ctorn = self.mung_ident(call.fn.identvalue.replace('.', '_'))
            print("{0}(".format(ctorn), end='')
            l = len(call.data)
            o = 0
            for i in call.data:
                self.generate_dispatch(i, depth=depth)
                if o < (l - 1):
                    print(", ", end='')
                o += 1
            print(")", end='')
        elif type(call) == CoastFNCallAST:
            print(self.mung_ident(str(call.fn)) + "(", end='')
            l = len(call.data)
            o = 0
            for i in call.data:
                self.generate_dispatch(i, depth=depth)
                if o < (l - 1):
                    print(", ", end='')
                o += 1
            print(")", end='')
        elif type(call) == CoastOpCallAST and call.op.identvalue == '|>':
            # XXX honestly we should have something that specializes here as
            # a general case, rather than this sort of test...
            oldast = None
            newast = None
            for idx in range(1, len(call.data)):
                if oldast is None:
                    oldast = call.data[idx - 1]
                newast = CoastFNCallAST(call.data[idx], [oldast])
                oldast = newast
            self.generate_fn_inverted_case(newast)
        elif type(call) == CoastOpCallAST and call.op.identvalue == '<|':
            oldast = None
            newast = None
            for idx in range(len(call.data) - 1, 0, -1):
                if oldast is None:
                    oldast = call.data[idx]
                newast = CoastFNCallAST(call.data[idx - 1], [oldast])
                oldast = newast
            self.generate_fn_inverted_case(newast)
        elif type(call) == CoastOpCallAST:
            op = call.op.identvalue
            print("(", end='')
            l = len(call.data)
            o = 0
            for i in call.data:
                self.generate_dispatch(i, depth=depth + 1)
                if o < (l - 1):
                    print(" {0} ".format(op), end='')
                o += 1
            print(")", end='')

    def generate_dispatch(self, ast, depth=0, tail=False):
        if type(ast) == CoastAssignAST and \
           (type(ast.value) == CoastFNAST or \
            type(ast.value) == CoastFCAST or \
            type(ast.value) == CoastGNAST):
            # NOTE: regardless, when we have an assignment, the
            # the final step of the `fn` form is the tail
            self.generate_fn(ast, depth=depth, tail=True)
        elif type(ast) == CoastAssignAST and \
             type(ast.value) == CoastCaseAST:
            # when we have an assignment off of a `case` form, we
            # actually want to invert the two, since `if` doesn't
            # introduce a new scope in Python, we can sorta trivially
            # abuse this
            #
            # this also makes me think that we can use this for rewriting
            # `case` forms in other areas, like function calls; we can pull
            # the `case` form out of the call, and assign it to a variable,
            # and then use that within the call itself, sort of like ANF.
            #
            # e.g.:
            #
            # `foo (bar case x | 10 ... esac);`
            #
            # can become:
            #
            # [source]
            # ----
            # bar_call0 = case x
            # | 10 ...
            # esac
            # foo (bar bar_call0);
            # ----
            #
            # in this way, we can rewrite things to a high-level ANF, and
            # still get decent performance out of it here
            self.generate_inverted_case(ast, depth=depth, tail=tail)
        elif type(ast) == CoastAssignAST:
            self.generate_assignment(ast, depth=depth)
        elif type(ast) == CoastFNCallAST or \
             type(ast) == CoastOpCallAST:
            if depth == 0:
                self.generate_fn_inverted_case(ast)
            else:
                self.generate_fn_inverted_case(ast, depth=depth, tail=tail)
        elif type(ast) == CoastCaseAST:
            self.generate_case(ast, depth=depth, tail=tail)
        elif type(ast) == CoastBlockAST:
            self.generate_block(ast, depth=depth+1, tail=tail)
        elif type(ast) == CoastTypeDefAST:
            self.generate_type(ast, depth=depth)
        elif type(ast) == CoastDeclareAST:
            print(self.mung_ident(ast.name), ": ", end="")
            self.generate_cardinal_type(ast.ntype)
        elif type(ast) is CoastLiteralAST and ast.littype is TokenArrayStart:
            if tail:
                print('return ', end='')
                self.generate_array(ast, depth=0)
            else:
                self.generate_array(ast, depth=depth)
        else:
            if tail:
                print('return ', end='')

            if type(ast) is CoastIdentAST:
                print(self.mung_ident(ast), end='')
            else:
                print(str(ast), end='')

    def generate(self, depth=0):
        # really what needs to happen here is that we
        # collect together all the generated python
        # and then return it; this way, we can send it
        # to a REPL or a file, or to the moon if we so
        # choose.

        # XXX really need to be more selective about these...
        print("from dataclasses import dataclass\nimport functools")
        print("import itertools\n")

        for ast in self.asts:
            self.generate_dispatch(ast, depth, tail=True)
            print("")

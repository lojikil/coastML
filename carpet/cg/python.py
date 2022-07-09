from ..parse import *

# The actual coastML -> Python compiler
# named after the "coastal carpet python"
class CarpetPython:
    def __init__(self, src, indent="    ", fh=None):
        self.fns = {}
        self.vals = {}
        self.src = src
        self.asts = []
        self.indent = indent
        self.res_ctr = 0
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
        self.asts = parser.parse()
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

    def is_basis_fn(self, fn):
        # is this one of the basis functions we know
        # how to optimize away?
        basislib = ["array-length", "array-get", "array-set!", "array-make",
                    "array-init", "array-make-matrix", "array-append",
                    "array-append!", "array-concat", "array-concat!",
                    "array-sub", "array-copy", "array-fill!", "array-blit!",
                    "array->list", "list->array", "array-iter",
                    "array-map", "array-iter-index", "array-map-index",
                    "array-foldl", "array-foldr", "array-sort", "array-sort!",
                    "array-stable-sort", "array-fast-sort", "string-length",
                    "string-get", "string-make", "string-init", "string-split",
                    "string-append", "string-join", "string-contains",
                    "string-concat", "string-copy", "string->array",
                    "string-iter", "string-map", "string-iter-index",
                    "string-map-index", "string-foldl", "string-foldr",
                    "string-sort", "compare", "char-code", "char-chr",
                    "char-escaped", "char-lowercase", "char-uppercase",
                    "char-compare"]
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
        params = ", ".join([x.to_coast() for x in v.parameters])
        print("def {0}({1}):".format(n, params))
        self.generate_block(v.body, tail=tail)

    def generate_assignment(self, v, depth=0):
        n = v.name.identvalue
        v = v.value
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
            self.generate_indent(depth + 1)
            if tail and o == (l - 1):
                self.generate_dispatch(b, depth=depth+1, tail=True)
            else:
                self.generate_dispatch(b, depth=depth+1)
            print("")
            o += 1

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
        elif t.basetype == "num":
            # NOTE: must remember to include `from numbers import Number`
            print("Number", end='')
        elif t.basetype == "array":
            print("list", end='')
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

    def generate_fn_inverted_case(self, ast, depth=0, tail=False):
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
                # NOTE there's probably nothing stopping us from just generating
                # in place here, but for now we actually iterate through twice
                lifted.append(newassign)
                newast.data.append(varast)
                # TODO this doesn't handle nested call-case combinations, need to
                # fix that; what we should do is process a call as a top-level form
                # return all bound `case` forms, then process those in toto
                #
                # I also wonder how often this will come up in practice, but it's good
                # to support it really
            else:
                # here, we just need to copy the argument to the new AST
                newast.data.append(arg)

        # and last step, we need to just generate the AST as normal
        for l in lifted:
            self.generate_inverted_case(l, depth=depth, tail=tail)

        self.generate_dispatch(newast)

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
                self.generate_indent(depth)
                print("{0} = ".format(resv), end='')
                self.generate_call(case.initial_condition, depth=0, tail=False)
                print("")
            else:
                resv = case.initial_condition.identvalue

            for cnd in case.conditions:
                test = cnd[0]
                then = cnd[1]
                bindings = None
                if type(test) is CoastIdentAST and test.identvalue == "_":
                    self.generate_indent(depth)
                    print("else:")
                    self.generate_block(then, depth=depth, tail=tail)
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
                            print("{0} = {1}", binding[0], binding[1])

                    self.generate_block(then, depth=depth, tail=tail)
                else:
                    if ctr > 0:
                        self.generate_indent(depth)
                        print('elif ', end='')
                    else:
                        print('if ', end='')
                    print('{0} == '.format(resv), end='')
                    self.generate_dispatch(test, depth=0, tail=False)
                    print(':')
                    self.generate_block(then, depth=depth, tail=tail)
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
            self.generate_fn(ast, depth=depth, tail=tail)
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
                self.generate_call(ast, depth=depth)
            else:
                self.generate_call(ast, depth=depth, tail=tail)
        elif type(ast) == CoastCaseAST:
            self.generate_case(ast, depth=depth, tail=tail)
        elif type(ast) == CoastBlockAST:
            self.generate_block(ast, depth=depth+1, tail=tail)
        elif type(ast) == CoastTypeDefAST:
            self.generate_type(ast, depth=depth)
        elif type(ast) is CoastLiteralAST and ast.littype is TokenArrayStart:
            if tail:
                print('return ', end='')
                self.generate_array(ast, depth=0)
            else:
                self.generate_array(ast, depth=depth)
        else:
            if tail:
                print('return', str(ast), end='')
            else:
                print(str(ast), end='')

    def generate(self, depth=0):
        # really what needs to happen here is that we
        # collect together all the generated python
        # and then return it; this way, we can send it
        # to a REPL or a file, or to the moon if we so
        # choose.
        for ast in self.asts:
            self.generate_dispatch(ast, depth, tail=True)
            print("")

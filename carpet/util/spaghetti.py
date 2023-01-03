class SpaghettiStack:
    # Spaghetti Stack is a simple stack-like
    # structure meant to capture environment
    # frames easily. Wikipedia has them labeled
    # "parent pointer trees" (https://en.wikipedia.org/wiki/Parent_pointer_tree)
    # but personally I've never heard that term.
    __slots__ = ["frames", "depth"]

    def __init__(self, env):
        self.frames = [env]
        self.depth = 1

    def copy(self, add_depth=True):
        # NOTE this actually *adds* one to the depth
        # of the frame, since that's the general use
        # case, but we allow users to set `add_depth`
        # to `False` in order to avoid that behavior
        if add_depth:
            ret = SpaghettiStack(None)
            ret.frames = [x for x in self.frames]
            if type(self.frames[0]) is list:
                ret.frames.append([])
            else:
                ret.frames.append({})
            ret.depth = self.depth + 1
            return ret
        else:
            ret = SpaghettiStack(None)
            ret.frames = [x for x in self.frames]
            ret.depth = self.depth
            return ret

    def __setitem__(self, key, item):
        self.frames[self.depth - 1][key] = item

    def __getitem__(self, key):
        for fidx in range(len(self.frames) - 1, -1, -1):
            frame = self.frames[fidx]
            if key in frame:
                return frame[key]
        raise KeyError(key)

    def __contains__(self, key):
        for fidx in range(len(self.frames) - 1, -1, -1):
            frame = self.frames[fidx]
            if key in frame:
                return True
        return False

    def __repr__(self):
        return repr(self.frames)

    def __str__(self):
        return str(self.frames)

    def __unicode__(self):
        return unicode(repr(self.frames))

    # NOTE these return ALL frame values
    # I was thinking there could be useful
    # versions that only return _scoped_ items
    # here. You could easily rebuild the dict
    # of what the environment looks like at
    # any given time
    # XXX add a `scoped=True` to `values` and
    # `items` so you can get *only* those values
    # that the environment would see at that time
    # this can help with debugging what `__getitem__`
    # is seeing for a particular stack frame
    def keys(self):
        ret = set()
        for fidx in range(len(self.frames) - 1, -1, -1):
            frame = self.frames[fidx]
            for key in frame.keys():
                ret.add(key)
        return list(ret)

    def values(self):
        ret = []
        for fidx in range(len(self.frames) - 1, -1, -1):
            frame = self.frames[fidx]
            for value in frame.values():
                ret.append(value)
        return ret

    def items(self):
        ret = []
        for fidx in range(len(self.frames) - 1, -1, -1):
            frame = self.frames[fidx]
            ret.append(frame.items())
        return ret

    def __ior__(self, rhs):
        if type(self.frames[0]) is dict:
            return self.frames[-1] | rhs
        else:
            raise NotImplemented("__ior__ only works on dictionary frames")

    def __iadd__(self, rhs):
        if type(self.frames[0]) is list:
            self.frames[-1] += rhs
            return self
        else:
            raise NotImplemented("__iadd__ only works on list frames")

# Just a simple helper to model how we actually use SpaghettiStacks in code really
# Also makes it easier when calling the sub-compiler, because instead of passing in
# 6 things that need to have their `copy` method called, we just pass in one thing
# that co-ordinates the whole affair
class EnvironmentFrame:
    def __init__(self, declarations, variables, functions, types, constructors, modules):
        self.declarations = SpaghettiStack(declarations)
        self.variables = SpaghettiStack(variables)
        self.functions = SpaghettiStack(functions)
        self.types = SpaghettiStack(types)
        self.constructors = SpaghettiStack(constructors)
        self.modules = SpaghettiStack(modules)
        self.depth = 0

    def copy(self):
        ret = EnvironmentFrame(self.declarations.copy(),
                               self.variables.copy(),
                               self.functions.copy(),
                               self.types.copy(),
                               self.constructors.copy(),
                               self.modules.copy())
        ret.depth = self.depth + 1
        return ret

    def __contains__(self, key):
        return (key in self.declarations) or \
               (key in self.variables) or \
               (key in self.functions) or \
               (key in self.types) or \
               (key in self.constructors) or \
               (key in self.modules)

    def __str__(self):
        tmp = ["Declaration: " + str(self.declarations),
               "Variables: " + str(self.variables),
               "Functions: " + str(self.functions),
               "Types: " + str(self.types),
               "Contructors: " + str(self.constructors),
               "Modules: " + str(self.modules)]
        return "EnvironmentFrame({0})".format(', '.join(tmp))


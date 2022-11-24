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



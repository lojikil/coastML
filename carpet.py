#@(#) the bootstrap compiler/parser/lexer for coastML
#@(#) written in Python3, mostly because I'm on a Python kick
#@(#) Ideally, we should:
#@(#)
#@(#) . generate Python3, C, Golang
#@(#) . have an AST Walker Interpreter
#@(#) . get rid of this as soon as possible
#@(#)
#@(#) Currently written as a single file, just to keep things
#@(#) relatively self-contained
#@(#) Thinking about it further, we can just clean this up and
#@(#) use it for the Python system, or at least the coastML->Python
#@(#) system. We can rewrite a better one in coastML later.
#@(#) Additionally, since we want to have a Python compiler, there's
#@(#) no real need to have an AST walker: just general Python3 from
#@(#) the REPL and evaluate it in Python itself...

import re
import string
import functools

class Token:
    def __repr__(self):
        return "Token()"

    def __str__(self):
        return "Token()"

class TokenEOF(Token):
    def __repr__(self):
        return "TokenEOF()"

    def __str__(self):
        return "TokenEOF()"

class TokenComment(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenComment({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme


# TokenCut 2

class TokenCut(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenCut()"

    def __str__(self):
        return self.lexeme

# TokenUnit 2 

class TokenUnit(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenUnit()"

    def __str__(self):
        return self.lexeme


# TokenIdent 3

class TokenIdent(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenIdent({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenCallStart 2

class TokenCallStart(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenCallStart()"

    def __str__(self):
        return self.lexeme

# TokenCallEnd 2

class TokenCallEnd(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenCallEnd()"

    def __str__(self):
        return self.lexeme

# TokenArrayStart 2

class TokenArrayStart(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenArrayStart()"

    def __str__(self):
        return self.lexeme

# TokenArrayEnd 2

class TokenArrayEnd(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenArrayEnd()"

    def __str__(self):
        return self.lexeme

# TokenBlockStart 2

class TokenBlockStart(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenBlockStart()"

    def __str__(self):
        return self.lexeme

# TokenBlockEnd 2

class TokenBlockEnd(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenBlockEnd()"

    def __str__(self):
        return self.lexeme

# TokenComma 2

class TokenComma(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenComma()"

    def __str__(self):
        return self.lexeme

# TokenSemiColon 2

class TokenSemiColon(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenSemiColon()"

    def __str__(self):
        return self.lexeme

# TokenSet 2

class TokenSet(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenSet()"

    def __str__(self):
        return self.lexeme

# TokenModSep 2

class TokenModSep(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenModSep()"

    def __str__(self):
        return self.lexeme

# TokenColon 2

class TokenColon(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenColon()"

    def __str__(self):
        return self.lexeme

# TokenError 3

class TokenError(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenError({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenChar 3

class TokenChar(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenChar({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenString 3

class TokenString(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenString({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenBin 3

class TokenBin(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenBin({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenOct 3

class TokenOct(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenOct({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenHex 3

class TokenHex(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenHex({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenInt 3

class TokenInt(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenInt({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenFloat 3

class TokenFloat(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenFloat({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenKeyword 3

class TokenKeyword(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenKeyword({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenType 3

class TokenType(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenType({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenBool 3

class TokenBool(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenBool({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenNSADT 3

class TokenNSADT(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenNSADT({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenNSMod 3

class TokenNSMod(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenNSMod({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenTag 3

class TokenTag(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenTag({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

# TokenOperator 3

class TokenOperator(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenOperator({0})".format(self.lexeme)

    def __str__(self):
        return self.lexeme

class Lex:
    def __init__(self, src=None, offset=0, line=0):
        self.src = src
        self.offset = offset
        self.line = line
        self.rest_ident = re.compile("[a-zA-Z0-9_+=!@$%^&*|?:\.<>-]")
        # we have two options here:
        #
        # * make tag/ident strict
        # * check them last...
        #
        # I'm going todo the latter, but it's a good thing to
        # note; Python's `re.match` will actually match `tag`
        # and `ns_mod` for the same, albeit a substring of the
        # actual string for the former
        self.tag = re.compile("[A-Z][a-zA-Z0-9_+=!@$%^&*|?<>-]*")
        self.ident = re.compile("[a-z_!@$%^&*<>][a-zA-Z0-9_+=!@$%^&*|?<>-]*")
        self.ns_adt = re.compile("[A-Z][a-zA-Z0-9_+=!@$%^&*|?<>-]*(\.[A-Z][a-zA-Z0-9_+=!@$%^&*|?<>-])+")
        self.ns_mod = re.compile("[A-Z][a-zA-Z0-9_+=!@$%^&*|?<>-]*(::[a-zA-Z0-9_+=!@$%^&*|?<>-])+")
        self.operators = re.compile("^([+=!@$%^&*|?<>-])+$")
        self.keywords = re.compile("^(case|esac|fn|fc|cf|gn|type|epyt|mod)$")
        self.types = re.compile("^(int|float|number|string|list|array|deque|function|unit)$")
        self.bools = re.compile("^(true|false)$")

    def next(self):
        o = self.offset
        if o >= len(self.src):
            return TokenEOF()
        elif self.src[o] in string.whitespace:
            while o < len(self.src) and self.src[o] in string.whitespace:
                if self.src[o] == '\n':
                    self.line += 1
                o += 1

        if o >= len(self.src):
            return TokenEOF()

        # ok, now we've reached here, let's start lexing out some lexemes
        if self.src[o] == '#':
            no = o
            while no < len(self.src) and self.src[no] != '\n':
                no += 1
            self.offset = no
            self.line += 1
            # Please note that we're returning a Comment token here
            # just so that we can also generate comments in the
            # target language
            return TokenComment(self.src[o:no], self.line, self.offset)
        # Let's start breaking down ideas here:
        # we need:
        #
        # . namespaced items: `+Foo.Bar+`
        # . accessors: `+_Foo+`
        # . Keywords: (linguistic & type): `+case+`, `+esac+`
        # . Tags: `+[A-Z][a-z0-9-+=!@$%^&*]+`
        # . Identifiers: `+[a-z][a-z0-9-+=!@$%^&*]+`
        # . Numbers: `+0b[0-1]+`, `+0o[0-7]+`, `+0x[0-9A-Fa-f]+`, `+[0-9]+`...
        # . Dividers: {} [] () $()
        elif self.src[o] == '$':
            if self.src[o + 1] == '(':
                self.offset += 2
                return TokenCut(self.line, self.offset)
            elif self.rest_ident.match(self.src[o + 1]):
                no = o + 1
                while no < len(self.src) and self.rest_ident.match(self.src[no]):
                    no += 1
                self.offset = no
                return TokenIdent(self.src[o:no], self.line, self.offset)
        elif self.src[o] == '(':
            if self.src[o + 1] == ')':
                self.offset += 2
                return TokenUnit(self.line, self.offset)
            self.offset = o + 1
            return TokenCallStart(self.line, self.offset)
        elif self.src[o] == ')':
            self.offset = o + 1
            return TokenCallEnd(self.line, self.offset)
        elif self.src[o] == '[':
            self.offset = o + 1
            return TokenArrayStart(self.line, self.offset)
        elif self.src[o] == ']':
            self.offset = o + 1
            return TokenArrayEnd(self.line, self.offset)
        elif self.src[o] == '{':
            self.offset = o + 1
            return TokenBlockStart(self.line, self.offset)
        elif self.src[o] == '}':
            self.offset = o + 1
            return TokenBlockEnd(self.line, self.offset)
        elif self.src[o] == ',':
            self.offset = o + 1
            return TokenComma(self.line, self.offset)
        elif self.src[o] == ';':
            self.offset = o + 1
            return TokenSemiColon(self.line, self.offset)
        elif self.src[0] == ':':
            no = o + 1
            if self.src[no] == '=':
                self.offset = no + 1
                return TokenSet(self.line, no)
            elif self.src[no] == ':':
                self.offset = no + 1
                # I don't believe we should make it here, but still...
                return TokenModSep(self.line, no)
            else:
                self.offset = no
                return TokenColon(self.line, self.offset)
        elif self.src[o] == "'":
            no = o + 1
            if self.src[no] == '\\':
                no += 1
            if self.src[no + 1] != "'":
                return TokenError("Incorrectly formatted character", self.line, self.offset)
            self.offset = no + 1
            return TokenChar(self.src[o:no + 1], self.line, self.offset)
        elif self.src[o] == '"':
            no = o + 1
            while no < len(self.src) and self.src[no] != '"':
                # normally we'd have something like `self.src[no] == '\\'`
                # but I don't actually think we care here... maybe
                # we could check if they're valid escapes, but... the only
                # one we care about here _really_ is an escaped `"`
                if self.src[no] == '\\':
                    no += 1
                if self.src[no] == '\n':
                    self.line += 1
                no += 1
            no += 1
            self.offset = no
            return TokenString(self.src[o:no], self.line, self.offset)
        elif self.src[o] == '0':
            # I wonder if it's easier to just use a general numeric
            # case here and then test format? Honestly very similar
            # to what I'm doing for idents/tags/keywords below...
            no = o + 1
            if no >= len(self.src):
                self.offset = no
                return TokenInt(self.src[o:no], self.line, self.offset)
            elif self.src[no] == 'b':
                no += 1
                while no < len(self.src) and self.src[no] in '01':
                    no += 1
                self.offset = no
                return TokenBin(self.src[o:no], self.line, self.offset)
            elif self.src[no] == 'o':
                no += 1
                while no < len(self.src) and self.src[no] in '01234567':
                    no += 1
                self.offset = no
                return TokenOct(self.src[o:no], self.line, self.offset)
            elif self.src[no] == 'x':
                no += 1
                while no < len(self.src) and self.src[no] in '0123456789ABCDEFabcdef':
                    no += 1
                self.offset = no
                return TokenHex(self.src[o:no], self.line, self.offset)
            elif self.src[no] in '0123456789':
                no += 1
                while no < len(self.src) and self.src[no] in '0123456789':
                    no += 1
                self.offset = no
                return TokenInt(self.src[o:no], self.line, self.offset)
            elif self.src[no] == '.':
                no += 1
                while no < len(self.src) and self.src[no] in '0123456789':
                    no += 1
                self.offset = no
                return TokenFloat(self.src[o:no], self.line, self.offset)
            elif self.src[no] in "()[];,\n\r ":
                self.offset = no
                return TokenInt(self.src[o:no], self.line, self.offset)
            else:
                return TokenError("Incorrectly formatted atom/numeral", self.line, self.offset)
        elif self.src[o] in '123456789':
            no = o + 1

            if no >= len(self.src):
                self.offset = no
                return TokenInt(self.src[o:no], self.line, self.offset)
            elif self.src[no] in '0123456789':
                no += 1
                while no < len(self.src) and self.src[no] in '0123456789':
                    no += 1
                self.offset = no
                return TokenInt(self.src[o:no], self.line, self.offset)
            elif self.src[no] == '.':
                no += 1
                while no < len(self.src) and self.src[no] in '0123456789':
                    no += 1
                self.offset = no
                return TokenFloat(self.src[o:no], self.line, self.offset)
            elif self.src[no] in "()[];,\n\r ":
                self.offset = no
                return TokenInt(self.src[o:no], self.line, self.offset)
            else:
                return TokenError("Incorrectly formatted numeral", self.line, self.offset)
        elif self.rest_ident.match(self.src[o]):
            no = o + 1
            while no < len(self.src) and self.rest_ident.match(self.src[no]):
                no += 1
            lexeme = self.src[o:no]
            self.offset = no
            # Now we're off to the races; we need to go through a few
            # different RegExs to see which ones matches...
            if self.keywords.match(lexeme):
                return TokenKeyword(lexeme, self.line, self.offset)
            elif self.types.match(lexeme):
                return TokenType(lexeme, self.line, self.offset)
            elif self.bools.match(lexeme):
                return TokenBool(lexeme, self.line, self.offset)
            elif self.ns_adt.match(lexeme):
                return TokenNSADT(lexeme, self.line, self.offset)
            elif self.ns_mod.match(lexeme):
                return TokenNSMod(lexeme, self.line, self.offset)
            elif self.operators.match(lexeme):
                return TokenOperator(lexeme, self.line, self.offset)
            elif self.tag.match(lexeme):
                return TokenTag(lexeme, self.line, self.offset)
            elif self.ident.match(lexeme):
                return TokenIdent(lexeme, self.line, self.offset)
            else:
                return TokenError("Malformed ident/tag/keyword", self.line, self.offset)

    def __iter__(self):
        return self

    def __next__(self):
        l = self.next()
        if type(l) == TokenError or type(l) == TokenEOF:
            raise StopIteration
        return l

class CoastAST:
    def __init__(self):
        pass

    def __repr__(self):
        return "CoastAST({0})".format(self.to_coast())

    def to_coast(self, depth=0):
        return "Coast {0}".format(depth)

    def indent(self, depth):
        res = ""
        for i in range(0, depth):
            res += "    "
        return res

    def __str__(self):
        return self.to_coast()

class CoastAssignAST(CoastAST):
    def __init__(self, n, v):
        self.name = n
        self.value = v

    def to_coast(self, depth=0):
        return "{0} = {1}".format(self.name, str(self.value))

    def __str__(self):
        return self.to_coast()

class CoastFNAST(CoastAST):
    def __init__(self, p, b, types=None):
        self.parameters = p
        self.body = b
        self.types = types

    def to_coast(self, depth=0):
        params = " ".join([x.to_coast(depth=depth + 1) for x in self.parameters])

        if self.types is not None:
            params = "[{0}] {1}".format(" ".join(self.types), params)

        return "fn {0} {1}".format(params, self.body)

    def __str__(self):
        return self.to_coast()

class CoastGNAST(CoastAST):
    def __init__(self, p, b, types=None):
        self.parameters = p
        self.body = b
        self.types = types

    def to_coast(self, depth=0):
        params = " ".join(self.parameters)

        if types is not None:
            params = "[{0}] {1}".format(" ".join(self.types), params)

        return "gn {0} {1}".format(params, self.body)

    def __str__(self):
        return self.to_coast()

class CoastFCAST(CoastAST):
    def __init__(self, p, c, types=None):
        self.parameters = p
        self.conditions = c
        self.types = types

    def to_coast(self, depth=0):
        params = " ".join(self.parameters)
        conds = "\n| " + "\n| ".join([" ".join(x[0], "{ " + x[1] + " }") for x in self.conditions])

        if types is not None:
            params = "[{0}] {1}".format(" ".join(self.types), params)

        return "fc {0} {1}\ncf".format(params, conds)

    def __str__(self):
        return self.to_coast()

class CoastCaseAST(CoastAST):
    def __init__(self, ic, c):
        self.initial_condition = ic
        self.conditions = c

    def to_coast(self, depth=0):
        d = depth + 1
        conds = "\n| " + "\n| ".join([" ".join([x[0].to_coast(depth=d), x[1].to_coast(depth=d)]) for x in self.conditions])
        if self.initial_condition is not None:
            return "case {0} {1}\nesac".format(self.initial_condition, conds)
        else:
            return "case{0}\nesac".format(conds)

    def __str__(self):
        return self.to_coast()

class CoastOpCallAST(CoastAST):
    def __init__(self, op, data):
        self.op = op
        self.data = data

    def to_coast(self, depth=0):
        op = str(self.op)
        data = [x.to_coast(depth=depth + 1) for x in self.data]
        v = functools.reduce(lambda x,y: x + " " + op + " " + y, data)
        if depth > 0:
            return "({0})".format(v)
        return v

    def __str__(self):
        return self.to_coast()

class CoastFNCallAST(CoastAST):
    def __init__(self, fn, data):
        self.fn = fn
        self.data = data

    def to_coast(self, depth=0):
        fn = str(self.fn)
        data = " ".join([x.to_coast(depth=depth + 1) for x in self.data])
        template = "{0} {1}"
        if depth > 0:
            template = "({0} {1})"
        return template.format(fn, data)

    def __str__(self):
        return self.to_coast()

class CoastBlockAST(CoastAST):
    def __init__(self, progn):
        self.progn = progn

    def to_coast(self, depth=0):
        if depth == 0:
            joiner = "\n" + self.indent(1)
        else:
            joiner = ";\n" + self.indent(depth=depth + 1)
        progn = joiner.join([x.to_coast() for x in self.progn])
        progn = self.indent(1) + progn
        # we can maybe use depth for indent here?
        return "{{\n{0}\n}}".format(progn)

    def __str__(self):
        return self.to_coast()

class CoastLiteralAST(CoastAST):
    def __init__(self, littype, litval):
        self.littype = littype
        self.litvalue = litval

    def to_coast(self, depth=0):
        # switch on the type...
        # wait, we don't actually need
        # to switch on the type here,
        # but we do need to _store_ the
        # type for other compilers. For
        # example, the Golang system
        # needs to know if we're talking
        # about a binary integer, since
        # Golang doesn't have native binary
        # integers...
        return self.litvalue

    def __str__(self):
        return self.to_coast()

class CoastIdentAST(CoastAST):
    def __init__(self, identtype, identval):
        self.identtype = identtype
        self.identvalue = identval

    def to_coast(self, depth=0):
        # switch on the type...
        # same note as the above: no real
        # need to do anything here, just
        # return the literal we have
        return self.identvalue

    def __str__(self):
        return self.to_coast()

class CoastalParseError(Exception):
    def __init__(self, error_message, line):
        self.error_message = error_message
        self.line = line

class CoastalParser:
    def __init__(self, src):
        self.src = src
        self.current_offset = 0
        self.lexemes = []
        self.asts = []

    def simple_value(self, v):
        ts = [TokenChar, TokenString, TokenBin, TokenOct,
              TokenHex, TokenInt, TokenFloat, TokenBool]
        for t in ts:
            if isinstance(v, t):
                return True
        return False

    def is_assignment(self, o):
        return isinstance(o, TokenOperator) and o.lexeme == "="

    def is_callable(self, c):
        ts = [TokenIdent, TokenOperator, TokenNSMod, TokenTag, TokenNSADT]

        for t in ts:
            if isinstance(c, t):
                return True
        return False

    def parse_assignment(self):
        if not self.is_assignment(self.lexemes[self.current_offset + 1]):
            raise CoastalParseError("`parse_assignment` called in non-assign context", self.lexemes[self.current_offset].line)
        name_o = self.current_offset
        self.current_offset += 2
        value = self.sub_parse()
        name = CoastIdentAST(type(self.lexemes[name_o]), self.lexemes[name_o].lexeme)
        return CoastAssignAST(name, value)

    def parse_block(self):
        res = []
        self.current_offset += 1
        while True:
            if isinstance(self.lexemes[self.current_offset], TokenBlockEnd):
                break
            res.append(self.sub_parse())
        self.current_offset += 1

        return CoastBlockAST(res)

    def parse_callable(self):
        res = CoastIdentAST(type(self.lexemes[self.current_offset]),
                            self.lexemes[self.current_offset].lexeme)
        self.current_offset += 1
        return res

    def parse_simple_value(self):
        if self.simple_value(self.lexemes[self.current_offset]):
            self.current_offset += 1
            return CoastLiteralAST(type(self.lexemes[self.current_offset - 1]),
                                   self.lexemes[self.current_offset - 1].lexeme)

    def parse_array_literal(self):
        self.current_offset += 1
        res = []
        while self.current_offset < len(self.lexemes):
            if isinstance(self.lexemes[self.current_offset], TokenArrayEnd):
                break
            elif self.simple_value(self.lexemes[self.current_offset]):
                res.append(self.parse_simple_value())
            elif isinstance(self.lexemes[self.current_offset], TokenComma):
                # commas aren't required, but we parse them if we get
                # them
                self.current_offset += 1
            else:
                res.append(self.sub_parse())
        return CoastLiteralAST(TokenArrayStart, res)

    def parse_call(self, paren=False):
        subcaptures = []
        suboffset = 0
        # We need to figure out a simple way of detecting which of the two
        # cases we're in (see below in callable), as well as handling
        # parenthetical call vs "naked" call
        # it could be even simpler: just check right here what the
        # first two tokens are, and use those
        # right, so do a first pass over to collapse everything, then
        # we can p easily check what all the subcaptures are, and do
        # shunting yard from there...
        #
        # Additionally, we need to actually read all values here until
        # the ';' really. Some of these are documented in the current
        # failures file, but basically we need to make sure we grab the
        # full call here, for operators
        # ... huh, the function call setup works for that, maybe follow
        # the same thing as there?
        while self.current_offset < len(self.lexemes):
            if self.simple_value(self.lexemes[self.current_offset]):
                subcaptures.append(self.parse_simple_value())
            elif isinstance(self.lexemes[self.current_offset], TokenArrayStart):
                subcaptures.append(self.parse_array_literal())
            elif paren == False and isinstance(self.lexemes[self.current_offset], TokenSemiColon):
                self.current_offset += 1
                break
            elif paren == False and isinstance(self.lexemes[self.current_offset], TokenBlockEnd):
                # we don't increment current_offset here because a block needs to consume it
                break
            elif paren and isinstance(self.lexemes[self.current_offset], TokenCallEnd):
                self.current_offset += 1
                break;
            elif isinstance(self.lexemes[self.current_offset], TokenCallStart):
                self.current_offset += 1
                subcaptures.append(self.parse_call(paren=True))
            elif isinstance(self.lexemes[self.current_offset], TokenCut):
                subcaptures.append(self.parse_cut())
            elif self.is_callable(self.lexemes[self.current_offset]):
                subcaptures.append(self.parse_callable())
            else:
                res = self.sub_parse()
                subcaptures.append(res)

        if len(subcaptures) == 1:
            return subcaptures[0]
        elif isinstance(subcaptures[0], CoastLiteralAST):
            # parse an operator call here, use shunting yard
            op = subcaptures[1]
            args = []
            for i in range(0, len(subcaptures)):
                if i % 2 == 0:
                    args.append(subcaptures[i])
                elif subcaptures[i].identvalue == op.identvalue:
                    pass
                else:
                    raise CoastalParseError("Attempted to use mis-matched operators", subcaptures[i].line)
            return CoastOpCallAST(op, args)
        elif hasattr(subcaptures[0], "identtype") and \
             hasattr(subcaptures[1], "identtype") and \
             subcaptures[0].identtype == TokenIdent and \
             subcaptures[1].identtype == TokenOperator:
            # this should probably just be an ident check
            # that's the only _real_ ambiguity here...
            # we also need to make sure we can operate if a
            # cut, fn, fc, or gn is the first here...
            op = subcaptures[1]
            args = []
            for i in range(0, len(subcaptures)):
                if i % 2 == 0:
                    args.append(subcaptures[i])
                elif subcaptures[i].identvalue == op.identvalue:
                    pass
                else:
                    raise CoastalParseError("Attempted to use mis-matched operators", subcaptures[i].line)
            return CoastOpCallAST(op, args)
        else:
            # here we want to turn this into a CoastAST that we
            # can treat as an ident/literal value
            return CoastFNCallAST(subcaptures[0], subcaptures[1:])

    def parse_case(self):
        # we need to support case-as-case as well as case-as-cond
        # the case form in coastML is deceptively simple:
        #
        # [source]
        # ----
        # case x
        #     | 10 { print_endline "it's ten" }
        #     | 11 { print_endline "it's eleven" }
        #     | _  { print_endline "it's something else" }
        # esac
        # ----
        #
        # but in reality, it's actually a bit more:
        #
        # . we have guards that we have to parse
        # . if we don't have an initial condition, it's really a `cond`
        ic = None
        conditions = []
        if type(self.lexemes[self.current_offset]) == TokenCallStart:
            ic = self.parse_call(paren=True)
        elif type(self.lexemes[self.current_offset]) == TokenOperator and \
             self.lexemes[self.current_offset].lexeme == "|":
            ic = None
        elif type(self.lexemes[self.current_offset]) == TokenIdent or \
             type(self.lexemes[self.current_offset]) == TokenNSMod or \
             type(self.lexemes[self.current_offset]) == TokenTag:
            ic = self.parse_callable()
        else:
            print(self.lexemes[self.current_offset])
            raise CoastalParseError("incorrectly formatted `case` form",
                                  self.lexemes[self.current_offset].line)

        while self.current_offset < len(self.lexemes):
            if type(self.lexemes[self.current_offset]) == TokenOperator and \
               self.lexemes[self.current_offset].lexeme == "|":
                self.current_offset += 1
                if type(self.lexemes[self.current_offset]) == TokenCallStart:
                    self.current_offset += 1
                    c = self.parse_call(paren=True)
                elif self.simple_value(self.lexemes[self.current_offset]):
                    c = self.parse_simple_value()
                elif self.is_callable(self.lexemes[self.current_offset]):
                    c = self.parse_callable()
                else:
                    print(self.lexemes[self.current_offset])
                    raise CoastalParseError("case conditions must be a value or a call",
                                          self.lexemes[self.current_offset].line)
                b = self.parse_block()
                conditions.append([c, b])
            elif type(self.lexemes[self.current_offset]) == TokenKeyword and \
                 self.lexemes[self.current_offset].lexeme == "esac":
                break
            else:
                raise CoastParseError("incorrectly formatted `case` form",
                                      self.lexemes[self.current_offset].line)
        return CoastCaseAST(ic, conditions)

    def parse_cut(self):
        pass

    def parse_fc(self):
        pass

    def parse_fn(self):
        parameters = []
        body = None
        while not isinstance(self.lexemes[self.current_offset], TokenBlockStart):
            if not isinstance(self.lexemes[self.current_offset], TokenIdent):
                raise CoastalParseError("fn parameters *must* be followed by idents", self.lexemes[self.current_offset].line)
            l = self.lexemes[self.current_offset]
            parameters.append(CoastIdentAST(TokenIdent, l.lexeme))
            self.current_offset += 1
        body = self.parse_block()
        return CoastFNAST(parameters, body)

    def parse_gn(self):
        pass

    def parse_type(self):
        pass

    def sub_parse(self):
        if type(self.lexemes[self.current_offset]) == TokenComment:
            self.current_offset += 1
            return self.sub_parse()
        elif type(self.lexemes[self.current_offset]) == TokenSemiColon:
            self.current_offset += 1
            return self.sub_parse()
        elif type(self.lexemes[self.current_offset]) == TokenIdent:
            # could be a function call or an assignment
            if self.is_assignment(self.lexemes[self.current_offset + 1]):
                return self.parse_assignment()
            else:
                return self.parse_call()
        elif self.simple_value(self.lexemes[self.current_offset]):
            # function call or just literal...
            return self.parse_call()
        elif type(self.lexemes[self.current_offset]) == TokenCallStart:
            self.current_offset += 1
            return self.parse_call(paren=True)
        elif type(self.lexemes[self.current_offset]) == TokenKeyword:
            # could be a function call (like anonymous lambda application)
            # or another form...
            cur_lex = self.lexemes[self.current_offset]
            self.current_offset += 1
            if cur_lex.lexeme == "case":
                return self.parse_case()
            elif cur_lex.lexeme == "fn":
                return self.parse_fn()
            elif cur_lex.lexeme == "gn":
                return self.parse_gn()
            elif cur_lex.lexeme == "fc":
                return self.parse_fc()
            elif cur_lex.lexeme == "type":
                return self.parse_type()
        elif type(self.lexemes[self.current_offset]) == TokenArrayStart:
            # function call? like `[1 2 3] someOp [4 5 6]` but not
            # likely
            return self.parse_call()
        elif type(self.lexemes[self.current_offset]) == TokenBlockStart:
            # a block of some sort...
            return self.parse_block()
        elif type(self.lexemes[self.current_offset]) == TokenCut:
            return self.parse_cut()
        elif type(self.lexemes[self.current_offset]) == TokenCallStart:
            return self.parse_call(paren=True)
        else:
            print(self.lexemes[self.current_offset])
            raise CoastalParseError("Incorrect top-level form", self.lexemes[self.current_offset].line)

    def load(self):
        lexer = Lex(self.src)
        lexeme = lexer.next()
        while not isinstance(lexeme, TokenEOF):
            if isinstance(lexeme, TokenError):
                raise CoastalParseError(lexeme.lexeme, lexeme.line)
            self.lexemes.append(lexeme)
            lexeme = lexer.next()

    def parse(self, reparse=False, ignore_comments=False):
        # There are a few different things we need to parse at the
        # top level:
        #
        # . Assignments
        # . Function/Operator calls
        # . Modules (we can elide some of this for now...)
        # . fn/fc/gn/case
        #
        # I've thought about actually turning the whole file into
        # a Stream/List and then being able to backtrack on position,
        # rather than attempting to lex one by one. We can (and will)
        # still use RDP (or TDOP or Shunting Yard), but it will be
        # fairly directed by the fact that we already have a stream
        # of lexemes

        if reparse:
            self.asts = []
            self.lexemes = []
            self.current_offset = 0

        if len(self.lexemes) > 0:
            return self.asts

        self.load()

        if ignore_comments:
            # I could just not add these above, but I didn't want to get
            # into hairy conditionals...
            self.lexemes = list(filter(lambda x: type(x) != "TokenComment", self.lexemes))

        while self.current_offset < len(self.lexemes):
            self.asts.append(self.sub_parse())
        return self.asts

# The actual coastML -> Python compiler
# named after the "coastal carpet python"
class CarpetPython:
    def __init__(self, src, indent="    "):
        self.fns = {}
        self.vals = {}
        self.src = src
        self.asts = []
        self.indent = indent

    def load(self):
        self.fns = {}
        self.vals = {}
        self.asts = []
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

    def generate_indent(self, cnt):
        for i in range(0, cnt):
            print(self.indent)

    def generate_fn(self, fn, depth=0):
        n = fn.name.identvalue
        v = fn.value
        params = ", ".join([x.to_coast() for x in v.parameters])
        print("def {0}({1}):\n".format(n, params))

    def generate_assignment(self, v, depth=0):
        pass

    def generate_literal(self, v, depth=0):
        pass

    def generate_block(self, block, depth=0):
        # we need to track if this or the call
        # is in the tail position, and return
        # from there, or really any form...
        for b in block:
            self.generate(b, depth=depth+1)

    def generate_call(self, call, depth=0):
        if type(call) == CoastFNCallAST:
            print(str(call.fn) + "(", end='')
            l = len(call.data)
            o = 0
            for i in call.data:
                self.generate_dispatch(i, depth=depth)
                if o < (l - 1):
                    print(", ", end='')
                o += 1
            print(")", end='')
        elif type(call) == CoastOpCallAST:
            pass

    def generate_dispatch(self, ast, depth=0):
        if type(ast) == CoastAssignAST and \
           (type(ast.value) == CoastFNAST or \
            type(ast.value) == CoastFCAST or \
            type(ast.value) == CoastGNAST):
            self.generate_fn(ast, depth=detph)
        elif type(ast) == CoastAssignAST:
            self.generate_assignment(ast, depth=depth)
        elif type(ast) == CoastFNCallAST or \
             type(ast) == CoastOpCallAST:
            self.generate_call(ast, depth=depth)
        elif type(ast) == CoastCaseAST:
            self.generate_case(ast, depth=depth)
        #elif type(ast) == CoastTypeAST:
        #    self.generate_type(ast, depth=depth)
        else:
            print(str(ast), end='')

    def generate(self, depth=0):
        for ast in self.asts:
            self.generate_dispatch(ast, depth)

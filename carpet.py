#!/usr/bin/env python3
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

import sys
import re
import string
import functools
import io
from typing import Union

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
        return "$("

# TokenUnit 2 

class TokenUnit(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenUnit()"

    def __str__(self):
        return "()"


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
        return "("

# TokenCallEnd 2

class TokenCallEnd(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenCallEnd()"

    def __str__(self):
        return ")"

# TokenArrayStart 2

class TokenArrayStart(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenArrayStart()"

    def __str__(self):
        return "["

# TokenArrayEnd 2

class TokenArrayEnd(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenArrayEnd()"

    def __str__(self):
        return "]"

# TokenBlockStart 2

class TokenBlockStart(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenBlockStart()"

    def __str__(self):
        return "{"

# TokenBlockEnd 2

class TokenBlockEnd(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenBlockEnd()"

    def __str__(self):
        return "}"

# TokenComma 2

class TokenComma(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenComma()"

    def __str__(self):
        return ","

# TokenSemiColon 2

class TokenSemiColon(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenSemiColon()"

    def __str__(self):
        return ";"

# TokenSet 2

class TokenSet(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenSet()"

    def __str__(self):
        return "="

# TokenModSep 2

class TokenModSep(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenModSep()"

    def __str__(self):
        return "::"

# TokenColon 2

class TokenColon(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "TokenColon()"

    def __str__(self):
        return ":"

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
        self.rest_ident = re.compile("[a-zA-Z0-9_+=!@$%^&*|?:\.<>/-]")
        # we have two options here:
        #
        # * make tag/ident strict
        # * check them last...
        #
        # I'm going todo the latter, but it's a good thing to
        # note; Python's `re.match` will actually match `tag`
        # and `ns_mod` for the same, albeit a substring of the
        # actual string for the former
        self.tag = re.compile("[A-Z][a-zA-Z0-9_+=!@$%^&*|?<>/-]*")
        self.ident = re.compile("[a-z_!@$%^&*<>][a-zA-Z0-9_+=!@$%^&*|?<>/-]*")
        self.ns_adt = re.compile("[A-Z][a-zA-Z0-9_+=!@$%^&*|?<>/-]*(\.[A-Z][a-zA-Z0-9_+=!@$%^&*|?<>/-])+")
        self.ns_mod = re.compile("[A-Z][a-zA-Z0-9_+=!@$%^&*|?<>/-]*(::[a-zA-Z0-9_+=!@$%^&*|?<>/-])+")
        self.operators = re.compile("^([+=!@$%^&*|?<>/-])+$")
        self.keywords = re.compile("^(case|esac|fn|fc|cf|gn|type|epyt|mod|is|box)$")
        self.types = re.compile("^(int|float|number|string|list|array|deque|function|unit|bool|char)$")
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
            # NOTE this is a fix for allowing _multidigit_ floating
            # point numbers; originally you could have floats that
            # began with a single digit, but not with _multiple_ digits
            # my original tests were too facile! This came up when I was
            # coding for my own edification and wrote some Julian date
            # library
            floatflag = False
            if no >= len(self.src):
                self.offset = no
                return TokenInt(self.src[o:no], self.line, self.offset)
            elif self.src[no] in '0123456789':
                no += 1
                while no < len(self.src) and self.src[no] in '.0123456789':
                    if self.src[no] == '.' and floatflag:
                        return TokenError("malformed floating point with two '.' characters", self.line, self.offset)
                    elif self.src[no] == '.':
                        floatflag = True
                    no += 1
                self.offset = no

                if floatflag:
                    return TokenFloat(self.src[o:no], self.line, self.offset)
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
    def __init__(self, n, v, t=None):
        self.name = n
        self.value = v
        self.ntype = t

    def to_coast(self, depth=0):
        if self.ntype:
            return "{0} is {1} = {2}".format(self.name, self.ntype, self.value)
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
        if type(self.fn) is not CoastIdentAST:
            fn = self.fn.to_coast(depth=1)
        else:
            fn = str(self.fn)
        data = " ".join([x.to_coast(depth=depth + 1) for x in self.data])
        template = "{0} {1};"
        if depth > 0:
            template = "({0} {1})"
        elif len(data) == 0 and depth == 0:
            return "({0});".format(fn)
        elif len(data) == 0:
            return "({0})".format(fn)
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
        if type(self.litvalue) is list:
            vs = [x.to_coast(depth=depth+1) for x in self.litvalue]
            return "[" + " ".join(vs) + "]"
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

class CoastTypeAST(CoastAST):
    def __init__(self, basetype, typeparameters):
        self.basetype = basetype
        self.typeparameters = typeparameters

    def to_coast(self, depth=0):
        if self.typeparameters:
            tp = " ".join([x.to_coast() for x in self.typeparameters.litvalue])
            return "{0}[{1}]".format(self.basetype, tp)
        return "{0}".format(self.basetype)

    def __str__(self):
        return self.to_coast()

class CoastTypeDefAST(CoastAST):
    def __init__(self, typename, constructors, types=None):
        self.typename = typename
        self.constructors = constructors
        self.types = types

    def to_coast(self, depth=0):
        header = "type {0}".format(self.typename)

        if self.types is not None:
            types = " ".join([x.to_coast for x in self.types])
            header = "{0} [{1}]".format(header, types)

        ctors = []
        for ctorn, ctorp in self.constructors:
            if type(ctorp) is CoastLiteralAST and len(ctorp.litvalue) > 0:
                params = " ".join([x.to_coast() for x in ctorp.litvalue])
                ctors.append("| {0} is [{1}]".format(ctorn.to_coast(),
                                                     params))
            elif type(ctorp) is list and len(ctorp) > 0:
                print("# here in list for", ctorn)
                params = " ".join([x.to_coast() for x in ctorp])
                ctors.append("| {0} is [{1}]".format(ctorn.to_coast(),
                                                     params))
            else:
                ctors.append("| {0}".format(ctorn.to_coast()))

        return "{0}\n{1}\nepyt".format(header, "\n".join(ctors))

    def __str__(self):
        return self.to_coast()

# I'm on the fence as to where to put some of the ideas I have
# for example, I'd like to have lambda lifting/closure conversion
# in some sort of central place, but not tied too tightly to
# a specific implementation. Although thinking about it,
# perhaps lambda lifting is generic and closure conversion is
# specific. My current thinking is that these sorts of generic
# source to source transformations should go in the parser, and
# it should be able to generate new ASTs/coastML
#
# * We can have the non-updating version `lift_lambda` which returns an AST
# * The destructive version `lift_lambda!` which updates current AST...
# * Then, we can easily lift all lambdas and return source...
#
# This can be used for a few different transforms as well, and applied
# only when end users request it or under certain circumstances; for example,
# in Python we can't generate `lambda` for all things that are functions in
# coastML

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
        if not self.is_assignment(self.lexemes[self.current_offset + 1]) and \
           self.lexemes[self.current_offset + 1].lexeme != "is":
            raise CoastalParseError("`parse_assignment` called in non-assign context", self.lexemes[self.current_offset].line)
        name_o = self.current_offset
        nt = None
        self.current_offset += 1
        # parse a type first, then a value
        if type(self.lexemes[self.current_offset]) == TokenKeyword and \
           self.lexemes[self.current_offset].lexeme == "is":
            self.current_offset += 1
            nt = self.parse_cardinal_type()
            if not self.is_assignment(self.lexemes[self.current_offset]):
                raise CoastalParseError("`parse_assignment` called in non-assign context after `is`",
                                        self.lexemes[self.current_offset].line)
            self.current_offset += 1
        else:
            self.current_offset += 1
        value = self.sub_parse()
        name = CoastIdentAST(type(self.lexemes[name_o]), self.lexemes[name_o].lexeme)
        if nt:
            return CoastAssignAST(name, value, nt)
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

    def parse_type_array_literal(self):
        self.current_offset += 1
        res = []
        while self.current_offset < len(self.lexemes):
            if isinstance(self.lexemes[self.current_offset], TokenArrayEnd):
                self.current_offset += 1
                break
            elif isinstance(self.lexemes[self.current_offset], TokenComma):
                # commas aren't required, but we parse them if we get
                # them
                self.current_offset += 1
            else:
                res.append(self.parse_cardinal_type())
        return CoastLiteralAST(TokenArrayStart, res)

    def parse_array_literal(self):
        self.current_offset += 1
        res = []
        while self.current_offset < len(self.lexemes):
            if isinstance(self.lexemes[self.current_offset], TokenArrayEnd):
                self.current_offset += 1
                break
            elif self.simple_value(self.lexemes[self.current_offset]):
                res.append(self.parse_simple_value())
            elif type(self.lexemes[self.current_offset]) == TokenArrayStart:
                res.append(self.parse_array_literal())
            elif type(self.lexemes[self.current_offset]) == TokenCallStart:
                self.current_offset += 1
                res.append(self.parse_call(paren=True))
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

        if len(subcaptures) == 1 and not paren:
            return subcaptures[0]
        elif len(subcaptures) > 1 and \
             hasattr(subcaptures[1], "identtype") and \
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
            self.current_offset += 1
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
                self.current_offset += 1
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

    def parse_cardinal_type(self):
        # parse built-in types for Carpet...
        # there are a few different things to do here
        # and I do suspect what we'll do is if we're called
        # from something like an `is`, we'll parse Tags thusly
        # as well...
        #
        # NOTE probably need to add things like "uint64" and such here
        # too.
        basictypes = ["int", "char", "float", "number", "string", "unit", "bool"]
        compltypes = ["list", "array", "deque", "function"]

        if type(self.lexemes[self.current_offset]) == TokenType and \
           self.lexemes[self.current_offset].lexeme in basictypes:
            self.current_offset += 1
            return CoastTypeAST(self.lexemes[self.current_offset - 1].lexeme,
                                None)
        elif type(self.lexemes[self.current_offset]) == TokenType and \
             self.lexemes[self.current_offset].lexeme in compltypes:
            if type(self.lexemes[self.current_offset + 1]) != TokenArrayStart:
                raise CoastalParseError("complex types must be followed by an array",
                                        self.lexemes[self.current_offset].line)
            basecompl = self.lexemes[self.current_offset].lexeme
            self.current_offset += 1
            parameter = self.parse_type_array_literal()
            return CoastTypeAST(basecompl, parameter)
        elif type(self.lexemes[self.current_offset]) == TokenTag:
            # attempt to parse a complex User type here
            basecompl = self.lexemes[self.current_offset].lexeme
            self.current_offset += 1
            if type(self.lexemes[self.current_offset]) == TokenArrayStart:
                parameter = self.parse_type_array_literal()
            else:
                parameter = None
            return CoastTypeAST(basecompl, parameter)
        else:
            print(self.current_offset)
            print(self.lexemes[self.current_offset])
            raise CoastalParseError("types must be built-in or a Tag",
                                    self.lexemes[self.current_offset].line)

    def parse_type(self):
        typename = None
        constructors = []
        if type(self.lexemes[self.current_offset]) != TokenTag:
            raise CoastalParseError("type forms *must* be followed by a Tag",
                                    self.lexemes[self.current_offset].line)
        typename = CoastIdentAST(type(self.lexemes[self.current_offset]),
                                 self.lexemes[self.current_offset].lexeme)
        # so here, we need to read if we have parameterized types, and then
        # the individual constructors:
        #
        # type TAG ("[" TYPE+ "]") (| TAG ((name ":") TYPE)*)+ epyt
        #
        # so, in the spirit of minimalism, I am wondering if I even want the
        # `|` there; it makes it easier to parse that way, but I could use
        # `;` at the end of the constructor for the same reason... I think the
        # visual start is better, but it is tempting to experiment with...#

        self.current_offset += 1
        if type(self.lexemes[self.current_offset]) == TokenArrayStart:
            types = self.parse_array_literal()
        else:
            types = None

        # ok, so we're past the name, and we're past any potential types, now
        # we just need to parse the various constructors and types. I do
        # wonder if I want to bother with names, but I feel like numbered
        # accessors would get messy fast (although we obviously should allow
        # them for people who want to use ADTs-as-tuples, just like named
        # accessors should be allowed for people who wish to use
        # ADTs-as-records
        #
        # Interesting... I was talking with @hex0punk today, and I realized
        # another interesting style to try:
        #
        # [source]
        # ----
        # type Foo[A B]
        # | Result is [A]
        # | Error is [B]
        # epyt
        # ----
        #
        # it matches the style of other declarations more closely, and is
        # pretty nice to parse...
        #
        # [source]
        # ----
        # type Daffodil
        # | Assoc is [list[tuple[string Daffodil]]]
        # | Array is [list[Daffodil]]
        # | Int is [int]
        # | Float is [float]
        # | Null
        # | Bool is [bool]
        # | String is [string]
        # epyt
        # ----

        while self.current_offset < len(self.lexemes):
            if type(self.lexemes[self.current_offset]) == TokenOperator and \
               self.lexemes[self.current_offset].lexeme == "|":
                self.current_offset += 1
                if type(self.lexemes[self.current_offset]) == TokenTag:
                    constructortag = self.parse_callable()
                else:
                    raise CoastalParseError("constructors *must* be tags in `type` forms",
                                            self.lexemes[self.current_offset].line)

                if type(self.lexemes[self.current_offset]) == TokenKeyword and \
                   self.lexemes[self.current_offset].lexeme == "is":
                    self.current_offset += 1
                    constructortypes = self.parse_type_array_literal()
                elif (type(self.lexemes[self.current_offset]) == TokenOperator and \
                      self.lexemes[self.current_offset].lexeme == "|") or \
                     (type(self.lexemes[self.current_offset]) == TokenKeyword and \
                      self.lexemes[self.current_offset].lexeme == "epyt"):
                    constructortypes = []
                else:
                    print(constructortag, self.lexemes[self.current_offset])
                    raise CoastalParseError("constructor tags must be followed by `is`",
                                            self.lexemes[self.current_offset].line)
                # here, we basically just read whatever until we match a `|` or a `epyt`
                constructors.append([constructortag, constructortypes])
            elif type(self.lexemes[self.current_offset]) == TokenKeyword and \
                 self.lexemes[self.current_offset].lexeme == "epyt":
                self.current_offset += 1
                break
            else:
                print(self.current_offset)
                raise CoastalParseError("incorrectly formatted `type` form",
                                        self.lexemes[self.current_offset].line)

        return CoastTypeDefAST(typename, constructors, types=types)

    def sub_parse(self):
        if self.current_offset >= len(self.lexemes):
            raise CoastalParseError("End of file", self.lexemes[-1].line)
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
            elif type(self.lexemes[self.current_offset + 1]) == TokenKeyword and \
                 self.lexemes[self.current_offset + 1].lexeme == "is":
                return self.parse_assignment()
            else:
                return self.parse_call()
        elif self.simple_value(self.lexemes[self.current_offset]):
            # function call or just literal...
            return self.parse_call()
        elif type(self.lexemes[self.current_offset]) == TokenCallStart:
            # originally this was breaking for input like `(10 * 3) + 1;`,
            # so this is closer, we end up with a parse tree that has
            # both the paren and the rest of the operation, but it's
            # being detected as a `fn` call:
            # . `fn.op` == `(10 * 3)` and
            # . `fn.data` == `[+ 1]`
            # which is also wrong
            return self.parse_call()
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
            elif cur_lex.lexeme == "box":
                # this is basically just a function call
                return self.parse_box()
        elif type(self.lexemes[self.current_offset]) == TokenType:
            return self.parse_cardinal_type()
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
            o = self.lexemes[self.current_offset]
            l = self.lexemes[self.current_offset].line
            raise CoastalParseError("Incorrect top-level form {0}".format(str(type(o))), l)

    def load(self, skip_comments=True):
        self.lexemes = []
        self.current_offset = 0
        lexer = Lex(self.src)
        lexeme = lexer.next()
        while not isinstance(lexeme, TokenEOF):
            if isinstance(lexeme, TokenError):
                raise CoastalParseError(lexeme.lexeme, lexeme.line)
            # eventually we want to support generating
            # CommentASTs below, but for now we just want to
            # generate code. This will strip out all TokenComment
            # objects, and then later we can handle those edge cases
            if skip_comments and type(lexeme) == TokenComment:
                pass
            else:
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

        if len(self.asts) > 0:
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
        print("pass")
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
        if tail:
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

if __name__ == "__main__":
    print("main", sys.argv)
    if len(sys.argv) != 3:
        print("usage: carpet.py [command] [file]")
        print("commands:\nload - load a file, and dump the resulting coastML")
        print("python - dump python from a coastML file, without the compiler")
        print("cpython - dump python from a coastML file, with the compiler")
        print("note, the last two will be merged at some point")
        sys.exit(0)

    if sys.argv[1] == "load":
        print("loading:", sys.argv[2])
        with open(sys.argv[2]) as fh:
            src = fh.read()
            c = CoastalParser(src)
            c.load()
            print(c.parse())
    elif sys.argv[1] == "python":
        print("pythonizing:", sys.argv[2])
        with open(sys.argv[2]) as fh:
            src = fh.read()
            c = CarpetPython(src)
            c.load()
            c.generate()
    elif sys.argv[1] == "cpython":
        print("compiler + python: ", sys.argv[2])
    else:
        print("unknown command:", sys.argv[1])

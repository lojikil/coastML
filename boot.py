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

import re
import string

class Token:
    pass

class TokenEOF(Token):
    pass

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


class Lex:
    def __init__(self, src=None, offset=0, line=0):
        self.src = src
        self.offset = offset
        self.line = line
        self.rest_ident = re.compile("[a-zA-Z0-9_+=!@$%^&*|?:\.-]")
        # we have two options here:
        #
        # * make tag/ident strict
        # * check them last...
        #
        # I'm going todo the latter, but it's a good thing to
        # note; Python's `re.match` will actually match `tag`
        # and `ns_mod` for the same, albeit a substring of the
        # actual string for the former
        self.tag = re.compile("[A-Z][a-zA-Z0-9_+=!@$%^&*|?-]*")
        self.ident = re.compile("[a-z][a-zA-Z0-9_+=!@$%^&*|?-]*")
        self.ns_adt = re.compile("[A-Z][a-zA-Z0-9_+=!@$%^&*|?-]*(\.[A-Z][a-zA-Z0-9_+=!@$%^&*|?-])+")
        self.ns_mod = re.compile("[A-Z][a-zA-Z0-9_+=!@$%^&*|?-]*(::[a-zA-Z0-9_+=!@$%^&*|?-])+")
        self.keywords = re.compile("^(case|esac|fn|fc|cf|gn|type|epyt|mod)$")
        self.types = re.compile("^(int|float|number|string|list|array|deque)$")
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
            self.offset += 1
            return TokenCallStart(self.line, self.offset)
        elif self.src[o] == ')':
            self.offset += 1
            return TokenCallEnd(self.line, self.offset)
        elif self.src[o] == '[':
            self.offset += 1
            return TokenArrayStart(self.line, self.offset)
        elif self.src[o] == ']':
            self.offset += 1
            return TokenArrayEnd(self.line, self.offset)
        elif self.src[o] == '{':
            self.offset += 1
            return TokenBlockStart(self.line, self.offset)
        elif self.src[o] == '}':
            self.offset += 1
            return TokenBlockEnd(self.line, self.offset)
        elif self.src[o] == ',':
            self.offset += 1
            return TokenComma(self.line, self.offset)
        elif self.src[o] == ';':
            self.offset += 1
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
            self.offset = no
            return TokenString(self.src[o:no], self.line, self.offset)
        elif self.src[o] == '0':
            # I wonder if it's easier to just use a general numeric
            # case here and then test format? Honestly very similar
            # to what I'm doing for idents/tags/keywords below...
            no = o + 1
            if self.src[no] == 'b':
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
            else:
                return TokenError("Incorrectly formatted atom/numeral", self.line, self.offset)
        elif self.rest_ident.match(src[o]):
            no = o + 1
            while no < len(self.src) and self.rest_ident.match(src[no]):
                no += 1
            lexeme = self.src[o:no]
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
            elif self.tag.match(lexeme):
                return TokenTag(lexeme, self.line, self.offset)
            elif self.ident.match(lexeme):
                return TokenIdent(lexeme, self.line, self.offset)
            else:
                return TokenError("Malformed ident/tag/keyword", self.line, self.offset)

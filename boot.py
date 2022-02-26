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

class Lex:
    def __init__(self, src=None, offset=0, line=0):
        self.src = src
        self.offset = offset
        self.line = line
        self.rest_ident = re.compile("[a-zA-Z0-9-+=!@$%^&*|?]")
        self.tag = re.compile("[A-Z][a-zA-Z0-9-+=!@$%^&*|?]*")
        self.ident = re.compile("[a-z][a-zA-Z0-9-+=!@$%^&*|?]*")
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
        elif self.rest_ident.match(src[o]):

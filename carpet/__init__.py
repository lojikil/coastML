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
from .parse import *
from .cg.python import CarpetPython
from .cg.javascript import CarpetJavaScript
from .cg.compiler import Compiler

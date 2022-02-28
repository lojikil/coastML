src = """
foo = fn x y { # (1)
    # functions are just named bindings
    # there's no other real need for
    # syntax
    case x # (2)
        | 10 { print_endline "ok, x is 10" } # (3)
        | 11 { print_endline "ok, x is 11" }
        | (x >= y) { print_endline "ok, x is >= y" } # (4)
        | else { print_endline "oh no, x is none of the above" } # (5)
    esac # (6)
}
foo 10
foo 20
"""
tokens = []
from boot import Lex
ll = Lex(src)
while True:
    l = ll.next()
    if repr(l) == "TokenEOF()":
        break
    elif "TokenError" in repr(l):
        print(repr(l))
        break
    else:
        tokens.append(l)


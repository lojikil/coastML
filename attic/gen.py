#@(#) lazy person's way of generating a lot of classes:
#@(#) write the code, then grep for returns, then write
#@(#) the classes. This is why I want to make a functional
#@(#) language with ADTs to handle this sort of thing...
#@(#)
#@(#) Usage:
#@(#)
#@(#) `grep 'return Token' boot.py | awk 'BEGIN {FS="return "} {print $2}' > tokens`
#@(#) then `python3 gen.py`

tmpl3 = """
class {0}(Token):
    def __init__(self, c, l, o):
        self.lexeme = c
        self.line = l
        self.offset = o

    def __repr__(self):
        return "{0}({{0}})".format(self.lexeme)

    def __str__(self):
        return self.lexeme"""
tmpl2 = """
class {0}(Token):
    def __init__(self, l, o):
        self.line = l
        self.offset = o

    def __repr__(self):
        return "{0}()"

    def __str__(self):
        return self.lexeme"""

with open('tokens') as fh:
    data = fh.readlines()

generate_cache = []
for d in data:
    line = d.split('(')
    if line[0] in generate_cache:
        continue
    cnt = len(line[1].split(','))
    print("\n#", line[0], cnt)
    generate_cache.append(line[0])
    if cnt == 2:
        print(tmpl2.format(line[0]))
    elif cnt == 3:
        print(tmpl3.format(line[0]))

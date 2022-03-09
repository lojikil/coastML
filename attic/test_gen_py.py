import carpet
src='a = 10;\nb = 20;\nfoo = fn x y {\n    x + y\n};\nfoo a b;\n'
c = carpet.CarpetPython(src)
c.load()
c.generate()

src = """type Foo 
| Int is [int]
| String is [string]
| Null
epyt"""
import carpet
c = carpet.CoastalParser(src)
c.load()
c.sub_parse()

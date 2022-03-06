src = """a = fn x y {
    b = fn z {
        z + x + y + 10;
    };
    b 20;
};"""
import carpet
c = carpet.CoastalParser(src)
c.load()
f = c.sub_parse()

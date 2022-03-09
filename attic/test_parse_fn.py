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
print(f.to_coast())

c = carpet.CoastalParser('case x | 10 { print_endline "10"; } | _ { print_endline "something else"; } esac')
c.load()
f = c.sub_parse()
print(f.to_coast())

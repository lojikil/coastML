import carpet

try:
    c = carpet.CoastalParser("1 + 2 + 3;")
    c.load()
    f = c.sub_parse()
    print("single digit numerals are working")
except:
    print("single digit numerals are still failing")

try:
    c = carpet.CoastalParser("(10 + 20) * 30")
    c.load()
    f = c.sub_parse()
except:
    print("Parenthized at start of call is still failing")

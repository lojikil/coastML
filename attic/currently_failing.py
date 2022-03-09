import carpet

try:
    c = carpet.CoastalParser("1 + 2 + 3;")
    c.load()
    f = c.sub_parse()
    print("single digit numerals are working")
except Exception as e:
    print("single digit numerals are still failing:", e)

# need to parse all the way to the end here, even
# for operators...
try:
    c = carpet.CoastalParser("(10 + 20) * 30")
    c.load()
    f = c.sub_parse()
    if f.to_coast() == "(10 + 20) * 30":
        print("correctly parsed!")
    else:
        raise Exception("incorrectly parsed!")
except Exception as e:
    print("Parenthized at start of call is still failing:", e)

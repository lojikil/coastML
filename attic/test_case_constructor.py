import carpet
src = """case x
| (Result.Ok 10) { print "10"; }
| (Result.Ok 11) { print "11"; }
| (Result.Ok _) {print "something else"; }
| (Result.Err _) { print "oh no!"; }
esac"""
c = carpet.CarpetPython(src)
c.load()
c.generate()

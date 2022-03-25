src = """case x
| 10 { print "x is 10"; }
| 11 { print "x is 11"; }
| (x >= 12) { print "x is >= 12"; }
| _ { print "x is something else"; }
esac
"""

src1 = """
case (foo bar)
| 10 { print "10"; }
| 11 { print "11"; }
| ($1 > 11) { print "returned greater than 11"; }
| _ { print "something else"; }
esac"""

src2 = """
case
| (x > y) { print "x > y"; }
| (x < y) { print "x < y"; }
| _ { print "x == y"; }
esac"""

# This is interesting
# how do we want to have guard clauses where
# we are matching type constructors...
#
# hmm... well what about allowing nested constructions?
# like `(Result.some (_1 > 10))`; that keeps things relatively
# clean, but also allows us to guard these expressions nicely
src3 = """
case foo
| (Result.Some 10) { print "foo is 10"; }
| (Result.Some _) { print "foo is:" (_1 foo); }
| (Result.None) { print "foo is none"; }
esac"""

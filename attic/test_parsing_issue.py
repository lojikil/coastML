#@(#) demonstrate a parsing issue
#@(#) in the below, if the `esac` token is followed by a `;`
#@(#) we actually have an issue with the top level block; I
#@(#) fixed one of the off by ones, but we still need to circle
#@(#) back on getting correct action there...

src="""foo = fn x y {
    case
        | (x < y) { print "x less than y"; }
        | (x > y) { print "x greater than y"; }
        | _ { print "x equal to y"; }
    esac
    print "done"
}
foo 10 20;
foo 20 20;
"""
src1="""foo = fn x y {
    case
        | (x < y) { print "x less than y"; }
        | (x > y) { print "x greater than y"; }
        | _ { print "x equal to y"; }
    esac;
}
foo 10 20;
foo 20 20;
"""
src2="""foo = fn x y {
    case
        | (x < y) { print "x less than y"; }
        | (x > y) { print "x greater than y"; }
        | _ { print "x equal to y"; }
    esac;
    print "done"
}
foo 10 20;
foo 20 20;
"""

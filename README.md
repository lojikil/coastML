# Overview {#_overview}

*come relax on the coast, stay for a while*

A tiny experimental language that combines
[Yeti](https://github.com/mth/yeti) and
[carML](https://github.com/lojikil/carml) more directly.

## BLUF {#_bluf}

1.  Tiny: just a handful of keywords and a simple grammar

2.  Safe: generate code that is human readable, reasonable to edit, and
    safe

3.  Historical: take my lessons learnt from writing carML, and apply
    them here, to a smaller language

4.  With input from writing a fair amount of ReasonML of late

# Rationale {#_rationale}

The other day I was working [in some Python
code](https://github.com/lojikil/modern-micro-multics/blob/master/vm.py#L47),
and I wanted to use a `match` like form, without upgrading to Python
3.10. My thought process was:

1.  Can I use carML? no, I'd rather not dive into that codebase

2.  Can I use [Coconut?](http://coconut-lang.org/) no, I didn't like the
    resulting Python code

3.  Can I generate some of these cases? yes, but it would be nicer to
    just write something

I've been lamenting that I don't really like working on carML: the
codebase is unwieldy because of the change in direction I had; so I
started looking at Yeti again, and thought that I could write something
like Yeti that generated a few different languages, like Python3, and
not have to write those again.

# Name {#_name}

Previously, this was just eXperimental Language No. 30 (XL/30); I've
written a few languages since carML, but none were meant to be a
linguistic experiment. However, based on internal polling (of myself, my
older son, and my partner) I've renamed this to coastML soon enough,
with logo and nomenclature (come to the coast, stay a while).

# Example {#_example}

Please see `cur.coast` for the current set of examples (and our
parser/compiler's test file), but the simplest example is:

    foo = fn x y {  
        # functions are just named bindings
        # there's no other real need for
        # syntax
        case x 
            | 10 { print_endline "ok, x is 10" } 
            | 11 { print_endline "ok, x is 11" }
            | (x >= y) { print_endline "ok, x is >= y" } 
            | _ { print_endline "oh no, x is none of the above" } 
        esac 
    }
    foo 10 5;
    foo 20 5;

-   Functions are just bound variables, no special syntax
-   lighter weight `case` form
-   Pattern matches are just a simple value followed by a block (both
    required)
-   Guards are just function calls; will check to decompose types
-   The default case is actually just a catch all with `_`
-   closing a form is just the name of the form backwards, Algol-style

One other item to note is that [I finally decided to actually handle
operators](https://github.com/lojikil/carML/blob/master/docs/opprec.md)

# Forms {#_forms}

There are only a handful of forms in coastML:

-   `fn`: a lambda
-   `gn`: a generic lambda (for multi-methods)
-   `fc`: a case-lambda
-   `type`: for introducing records & ADTs
-   `case`: a combination of `cond` and `case` or `match`
-   `mod`: modules
-   type classes in the form of `sig` and `impl`
-   assignments, operators, and function calls
    -   one note about operators: like Project Verona and carML, coastML
        has no operator precedence, but does support inline operators
    -   This means that you cannot have two operators in a form that are
        of different precedence without parentheses
    -   for example: `1 + 2 + 3` is fine, but `1 + 2 * 3` requires
        parentheses: `1 + (2 * 3)`
    -   function calls work similarly: `print_endline (→string foo)`

# Current Usage {#_current_usage}

The top-level interfaces that most people would be interested in are:

1.  `carpet.Lex`, which can be used to iteratively tokenize all source
    code
2.  `carpet.CoastalParser`, which returns AST objects and
3.  `carpet.CarpetPython`, which actually generates Python code
4.  `python3 -m carpet`, which just runs the module's `main` code
5.  `coastml`, which is just a shell script wrapping the same

``` python
import carpet
src = """case x
| 10 { print "x is 10" }
| 11 { print "x is 11" }
| (x > y) { print "x is greater than y" }
| _ { print "none of the above" }
esac"""
ll = carpet.Lex(src)
for l in ll: 
    somefn(l)

coastparser = carpet.CoastalParser(src)
coastparser.load() 
srccaseast = coastparser.sub_parse() 

carpy = carpet.CarpetPython(src)
carpy.load()
carpy.generate() 
```

-   The lexical analyzer also includes a `next` method, you needn't use
    it via iterators
-   Both the parser and the python generator can be reset with their
    respective `load` methods
-   We use `sub_parse` here, but there is a `parse` method to return
    **all** ASTs
-   This currently just prints to screen, but I'll refactor it to
    generate a string

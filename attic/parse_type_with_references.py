src = """type Daffodil
| Assoc is [list[string] list[Daffodil]]
| Array is [list[Daffodil]]
| Int is [int]
| Float is [float]
| Null
| Bool is [bool]
| String is [string]
epyt"""

# ok, so the above is failing because we don't know how
# to handle Tags at the `sub_parse` level. In carML, we
# just returned a tag there, and later we figured out
# what we wanted to do in context. I do think tho that
# it may be worth adding a `parse_type_array` helper
# to handle these sorts of situations more gracefully...

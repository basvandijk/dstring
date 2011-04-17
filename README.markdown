Difference strings: a data structure for O(1) append on strings. Note
that a `DString` is just a newtype wrapper around a [DList] `Char`. The
reason we need a new type instead of just a type synonym is that we
can have an `instance IsString DString` without using language
extensions (`TypeSynonymInstances` or `FlexibleInstances`) so we can
write overloaded string literals of type `DString`.

[DList]: http://hackage.haskell.org/packages/archive/dlist/latest/doc/html/Data-DList.html#t:DList

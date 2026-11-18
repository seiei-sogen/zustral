Test that a union case cannot have the name of a record of its module (issue #86). Before, the case was accepted, and `Foo(v => 1)` was told that there is no type named `Foo`.

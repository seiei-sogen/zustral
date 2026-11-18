Test that a reference type whose region is a type, `&[Int32, Int32]`, is rejected (issue #90). Before, the type parser kept whatever was written in the region position, and the program compiled.

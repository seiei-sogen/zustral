Test that two records cannot be compared (issue #125). Before, `a = b` passed the type check, and the C compile failed with `invalid operands to binary ==`.

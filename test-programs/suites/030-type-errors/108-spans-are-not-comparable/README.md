Test that two spans cannot be compared (issues #125 and #144). Before, `"abc" = s` passed the type check, and the C compile failed with `invalid operands to binary ==`.

Test that two unions cannot be compared (issue #125). Before, `A() /= B()` passed the type check, and the C compile failed with `invalid operands to binary !=`.

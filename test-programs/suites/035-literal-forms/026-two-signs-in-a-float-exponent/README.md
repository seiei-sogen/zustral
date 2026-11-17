Pins that the exponent of a float literal has at most one sign: `1.5e+-3` does not parse. The compiler used to accept it and the C compiler failed with `exponent has no digits` (issue #47).

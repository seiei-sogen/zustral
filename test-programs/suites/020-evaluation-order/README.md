Tests that the operands of an operator and the arguments of a call are
evaluated in the order the program writes them (issue #53): from left to
right, and named arguments in the order of the call, not of the declaration.

Every program prints from inside its operands, so its output is the order. The
C compiler does not choose: gcc evaluates the arguments of a C call from the
last to the first, and clang from the first to the last.

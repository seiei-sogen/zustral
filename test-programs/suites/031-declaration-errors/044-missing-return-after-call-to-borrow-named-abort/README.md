Test that a `borrow` statement whose last statement calls the reference it named `abort` fails with the missing return: the name is a variable there, not the built-in function (issue #157).

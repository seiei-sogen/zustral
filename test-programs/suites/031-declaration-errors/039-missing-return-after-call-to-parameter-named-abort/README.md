Test that a function whose last statement calls a function pointer parameter named `abort` fails: the call returns, so the body does not end in a return (issue #157).

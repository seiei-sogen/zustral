Test that unary minus negates a float and doesn't truncate it to an integer (issue #26). The compiler used to read `-f` as `0 - f`, and an integer literal is not a float.

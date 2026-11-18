Test that a constraint of a type parameter that names nothing is rejected where it is declared, also when no call checks it (issue #90). Before, this program, which never calls `f`, compiled.

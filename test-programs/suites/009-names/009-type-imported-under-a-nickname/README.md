Test that a type imported under a nickname is the type it names (issue #50). Before, `let m: Md := Up();` failed with `Cannot cast to Md from Mode`.

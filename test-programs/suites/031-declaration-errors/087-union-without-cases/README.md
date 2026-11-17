Test that a union with no cases is rejected where it is declared (issues #163 and #133). Before, the program passed the compiler, and the C compiler failed with `empty enum is invalid`.

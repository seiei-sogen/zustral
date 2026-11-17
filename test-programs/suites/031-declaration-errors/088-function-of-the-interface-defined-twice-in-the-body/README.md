Test that the body of a module cannot define twice a function that its interface declares (issue #126). Before, the second definition was dropped without a word, and the program printed 1.

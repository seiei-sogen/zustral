Test that a `Foreign_Import` pragma before a record is rejected: only a function definition takes it (issue #93). Before, the pragma was dropped without a word, and the program printed 1.

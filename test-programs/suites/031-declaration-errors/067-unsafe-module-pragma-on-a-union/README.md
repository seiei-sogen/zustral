Test that an `Unsafe_Module` pragma before a union is rejected: it belongs before `module body` (issue #93). Before, the pragma was dropped without a word, and the module stayed safe.

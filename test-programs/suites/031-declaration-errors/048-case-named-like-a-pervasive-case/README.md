Test that a union case cannot have a name that `Zustral.Pervasive` imports into every module (issue #28). Before, `None()` built the imported `Option`, and the error said only that the types differ.

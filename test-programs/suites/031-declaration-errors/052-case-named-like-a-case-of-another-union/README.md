Test that two unions of a module cannot have a case of the same name (issue #86). Before, both cases were accepted, `Foo()` built a `V`, and the error said `Cannot cast to U from V`.

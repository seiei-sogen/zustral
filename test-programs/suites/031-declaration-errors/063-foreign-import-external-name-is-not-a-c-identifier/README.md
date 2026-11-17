Test that the `External_Name` of a `Foreign_Import` must be a C identifier (issues #92 and #161). The name is pasted into the C as written. Before, `not a c name!` passed, and the C compile failed.

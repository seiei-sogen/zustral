Pins that the escapes after a whitespace escape (`\ ... \`) are still read as escapes: `"A\ \B\nC"` is `AB`, a newline and `C`. The `\n` used to stay as two characters (issue #140).

A Lambda calculus tokenizer and parser implemented 
in Standard ML that outputs JavaScript.

Usage:
# navigate to the folder the sml files are in
# start the sml interpreter
> sml
- use "printjs.sml";
- printjs (parse "filename");

example:

printjs (parse "tests/test0.txt");

This will print the following javaScript:
(1 + 1)
This is a simple imperative language for testing concepts.

First iterations will be used to test classical parallel programming concepts (Hoare's monitors and semaphores).

Currently it's a totally dynamic language, the compiler just parses the code and generates bytecode without any checks, so programs are super likely to crash.

Later, if I find time, I want to add static type checking etc.

I might also use it to test other concepts in the future, or maybe someone else will want to fork it to test their ideas ;)

Docs:
 - [Runtime VM specs](docs/Dynamic.md)
 - the language has C-like syntax and is (currently) completely dynamic - there are no typechecks (YET) and variables can be allocated almost on the fly (I want to change that, as I don't like such dynamic languages, but I don't have enought time right now)

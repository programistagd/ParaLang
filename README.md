# ParaLang

This is a simple imperative language for testing concepts.

First iterations will be used to test classical parallel programming concepts (Hoare's monitors and semaphores).

Currently it's a totally dynamic language, the compiler just parses the code and generates bytecode without any checks, so programs are super likely to crash.

Later, if I find time, I want to add static type checking etc.

I might also use it to test other concepts in the future, or maybe someone else will want to fork it to test their ideas ;)

## Docs:
 - [Runtime VM specs](docs/Dynamic.md)
 - The language has C-like syntax and is (currently) completely dynamic - there are no typechecks (YET) and variables can be allocated almost on the fly (I want to change that, as I don't like such dynamic languages, but I don't have enough time right now).

## Getting it to work

#### Compiler
To use the compiler you need `cabal` (comes with `haskell-platform`).
 - Go to the Compiler/ directory.
 - Run
   - `cabal install --only-dependencies`
   - `cabal configure`
   - `cabal build` (optional, run will do this)

You can use `cabal run example.pal` to compile the example.pal file and print the bytecode into stdout (you can then redirect that to a file), it will also print the AST into stderr

If you are not developing the compiler right now, you can type `cabal install` and then use the compiler with the simple command `ParaCompiler`.

#### Runtime
I don't like the Gopath, because I keep both Haskell and Go in one repo (maybe it's not good, but it's a small project, didn't want to make it too divided).

My solution is to link `$GOPATH/src/github.com/programistagd/ParaLang/ParaRuntime` to `ParaRuntime` in the place you cloned the repo into (but you can just clone the repo into the Gopath if you like).

Then run `go install github.com/programistagd/ParaLang/ParaRuntime`.

If you have `$GOPATH/bin` in your PATH you should now be able to run `ParaRuntime`. If it gets no arguments or `-` it will read input from stdin, otherwise from the file specified.

This way you can pipe the output from the compiler into the runtime to compile and execute your code at once.

#### Putting it all together
If you have installed both the runtime and the compiler, the easiest option to run a script file is to run `ParaCompiler file.pal | ParaRuntime`.

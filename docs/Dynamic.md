##VM assumptions
 - For simplicity I assume the VM has infinite amount of registers
 - Currently there are no optimizations in usage of registers because I'm not doing this to learn compiler optimization (right now, that'll come but later)

##Scopes
Variables are stored in maps and there are several scope levels accessible:
- global scope
- struct scope (TODO)
- function scope

Lookups go from the most local scope to the most global.

In global variable definitions, only global scope is accessible (so it is the most local),
in free functions we check function scope and later global scope.


##Bytecode ref
 - `load name reg` - loads a variable to register `reg`
 - `save reg name` - saves register `reg` into a variable called name (if such variable exists at any scope it's updated, otherwise it's created at the most local scope)
 - `seti value reg` - loads integer value into register
 - `sets value reg` - loads string value into register
 - `BinOP regA regB regR` -> regR := regA OP regB
 - `puts str` - put a string constant onto the stack
 - `push reg` - put value from reg onto the stack
 - `pop reg` - pop a value from the stack and put it into reg
 - `call reg` - call a function from stack (gets its name and then all parameters from the stack) and put result into reg
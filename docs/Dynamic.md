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

##Function call convention
The VM handles function calls and makes sure the caller context and function context are separated by managing open scopes and registers. In practice that means that interpreting a function call on a simple VM can just call an interpreting function recursively.

##Structures
The VM supports objects of simple 'struct' type, managing their memory and function calls.
For struct 'xy' there is a global function 'xy' that is its constructor.

##Bytecode ref
 - `load name reg` - loads a variable to register `reg`
 - `save reg name` - saves register `reg` into a variable called name (if such variable exists at any scope it's updated, otherwise it's created at the most local scope)
 - `loada areg ireg rreg` - loads element at index `ireg` from array referenced by `areg` into `rreg`
 - `savea areg ireq vreg` - saves `vreg` into array `areg` at place `ireg` (array should be large enough!)
 - `seti value reg` - loads integer value into register
 - `sets value reg` - loads string value into register
 - `BinOP regA regB regR` -> regR := regA OP regB
 - `puts str` - put a string constant onto the stack
 - `push reg` - put value from reg onto the stack
 - `pop reg` - pop a value from the stack and put it into reg
 - `call reg` - call a function from stack (gets its name and then all parameters from the stack) and put result into reg
 - `jmp reg` - sets current op pointer so that the next operation executed will be (current + value of reg); Warning: `jmp (0)` is an infinite loop
 - `if reg amount` - if reg is equal to 0 acts like `jmp amount`, otherwise acts like a noop
 - `open objreg` - brings up objects scope as a temporary local scope
 - `close` - closes the open scope

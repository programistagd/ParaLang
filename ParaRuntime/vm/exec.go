package vm

import (
	"strconv"
	"strings"
)

/*
TODO
*/
func Exec(scope *Scope, state *CPUState, stack *CPUStack, line string) {
	state.currentOp++
	parts := strings.Split(line, " ")
	opcode := parts[0]
	narg := func(n int) int { //numeric args
		i, err := strconv.Atoi(parts[n])
		if err != nil {
			panic(err)
		}
		return i
	}
	idarg := func(n int) string { //identifier -> string without quotes (only letters and numbers)
		return parts[n]
	}
	switch opcode {
	case "load":
		state.registers[narg(2)] = scope.Get(idarg(1))
	case "save":
		scope.Set(idarg(2), state.registers[narg(1)])
	case "loada":
		panic("Not implemented")
	case "savea":
		panic("Not implemented")
	case "seti":
		state.registers[narg(2)] = ValueInt(narg(1))
	case "sets":
		panic("Not implemented")
	case "puts":
		stack.Push(ValueStr(idarg(1)))
	case "push":
		stack.Push(state.registers[narg(1)])
	case "pop":
		state.registers[narg(1)] = stack.Pop()
	case "call":
		id := stack.Pop().AsString()
		fun := scope.GetFunc(id)
		if fun == nil {
			panic("Calling not-declared function " + id)
		}
		nestedScope := MakeChildScope(scope)
		state.registers[narg(1)] = fun(&nestedScope, stack)
	case "jmp":
		jmp := state.registers[narg(1)].AsInt()
		state.currentOp = state.currentOp - 1 + jmp
	case "if":
		cond := state.registers[narg(1)].AsInt()
		if cond == 0 {
			jmp := state.registers[narg(2)].AsInt()
			state.currentOp = state.currentOp - 1 + jmp
		}
	default:
		panic("Unknown")
	}
}

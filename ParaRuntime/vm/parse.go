package vm

import (
	"strconv"
	"strings"
)

/*
TODO
*/
func ParseFile(lines []string) Scope {
	scope := MakeChildScope(GetBuiltinsScope())
	state := CPUState{}
	stack := CPUStack{}
	i := 0
	for i < len(lines) {
		line := lines[i]
		parts := strings.SplitN(line, " ", 2)
		op := parts[0]
		switch op {
		case "struct":
			name := parts[1]
			j := i + 1
			for lines[j] != "endstruct" {
				j++
			}
			strukt := parseStruct(lines[i+1 : j])
			scope.funs[name] = func(*Scope, *CPUStack) PValue {
				_ = strukt //TODO FIXME implement struct constructor
				return ValueNil()
			}
			i = j + 1
		case "func":
			name := parts[1]
			j := i + 1
			for lines[j] != "endfunc" {
				j++
			}
			fun := parseFunc(lines[i+1 : j])
			scope.funs[name] = fun
			i = j + 1
		default:
			Exec(&scope, &state, &stack, line)
		}
	}
	return scope
}

func parseStruct(lines []string) PStruct {
	s := PStruct{}
	return s
}

/*
TODO
*/
func parseFunc(lines []string) PFunc {
	return func(scope *Scope, stack *CPUStack) PValue {
		state := CPUState{}
		for state.currentOp < len(lines) { //TODO
			line := lines[state.currentOp]
			if strings.HasPrefix(line, "retrn") {
				i, err := strconv.Atoi(strings.SplitN(line, " ", 2)[1])
				if err != nil {
					panic(err)
				}
				return state.registers[i]
			}

			Exec(scope, &state, stack, line)
		}
		return ValueNil()
	}
}

package vm

import "strings"

/*
TODO
*/
func ParseFile(lines []string) Scope {
	scope := Scope{}
	i := 0
	for i < len(lines) {
		line := lines[i]
		parts := strings.SplitN(line, " ", 2)
		op := parts[0]
		switch op {
		case "struct":
			name := parts[1]
			//TODO
		case "func":
			name := parts[1]
			j := i + 1
			for lines[j] != "endfunc" {
				j++
			}
			fun := parseFunc(lines[i+1 : j])
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
	return func(scope *Scope, state *CPUState) {
		panic("Not implemented")
	}
}

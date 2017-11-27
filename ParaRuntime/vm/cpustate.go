package vm

/*
TODO
*/
type Scope struct {
	local  map[string]PValue
	funs   map[string]PFunc
	parent *Scope
}

/*
TODO
*/
type CPUState struct {
	registers [100]PValue //TODO autoresizing
	stack     []PValue
	opCount   int
}

package vm

/*
TODO
*/
type Scope map[string]PValue

/*
TODO
*/
type StackState struct {
	registers [100]PValue //TODO autoresizing
}

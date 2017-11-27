package vm

type valueType int

const (
	tInt valueType = iota
	tStr
	tStruct
)

/*
PValue is TODO
*/
type PValue struct {
	//not sure how to best solve this
	currentType valueType
	ival        int
	strval      string
	objval      *PStruct
}

/*
PFunc is TODO
*/
type PFunc func(*Scope, *CPUState)

/*
PStruct is TODO
*/
type PStruct struct {
	myScope     Scope
	myFunctions map[string]PFunc
}

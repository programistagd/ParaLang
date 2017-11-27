package vm

import "strconv"

type valueType int

const (
	tNothing valueType = iota
	tInt
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

func ValueNil() PValue {
	v := PValue{}
	v.currentType = tNothing // actually could probably return empty Pvalue, but I want to be explicit
	return v
}

func ValueInt(n int) PValue {
	v := PValue{}
	v.currentType = tInt
	v.ival = n
	return v
}

func ValueStr(s string) PValue {
	v := PValue{}
	v.currentType = tStr
	v.strval = s
	return v
}

func (v *PValue) ToString() string {
	switch v.currentType {
	case tNothing:
		return "null"
	case tStr:
		return "\"" + v.strval + "\""
	case tInt:
		return strconv.FormatInt(int64(v.ival), 10) // TODO investigate
	case tStruct:
		return "struct TODO"
	}
	return "???"
}

func (v PValue) AsString() string {
	if v.currentType != tStr {
		panic("Type mismatch")
	}
	return v.strval
}

func (v PValue) AsInt() int {
	if v.currentType != tInt {
		panic("Type mismatch")
	}
	return v.ival
}

/*
PFunc is TODO
*/
type PFunc func(*Scope, *CPUStack) PValue

/*
PStruct is TODO
*/
type PStruct struct {
	myScope     Scope
	myFunctions map[string]PFunc
}

package vm

/*
TODO
*/
type Scope struct {
	vars   map[string]PValue
	funs   map[string]PFunc
	parent *Scope
}

/*
TODO
*/
type CPUState struct {
	registers [100]PValue //TODO autoresizing
	currentOp int
}

type CPUStack struct {
	stack []PValue
}

func (stack *CPUStack) Push(v PValue) {
	stack.stack = append(stack.stack, v)
}

func (stack *CPUStack) Pop() PValue {
	lasti := len(stack.stack) - 1
	if lasti < 0 {
		panic("Stack underflow")
	}
	v := stack.stack[lasti]
	stack.stack = stack.stack[:lasti]
	return v
}

func MakeEmptyScope() Scope {
	s := Scope{}
	s.vars = make(map[string]PValue)
	s.funs = make(map[string]PFunc)
	return s
}

func MakeChildScope(parent *Scope) Scope {
	s := MakeEmptyScope()
	s.parent = parent
	return s
}

func (s *Scope) Get(id string) PValue {
	v, present := s.vars[id]
	if present {
		return v
	} else if s.parent != nil {
		return s.parent.Get(id)
	}
	return ValueNil()
}

func (s *Scope) Has(id string) bool {
	_, present := s.vars[id]
	if present {
		return true
	} else if s.parent != nil {
		return s.parent.Has(id)
	}
	return false
}

func (s *Scope) Set(id string, val PValue) {
	_, present := s.vars[id]
	if !present && s.parent != nil && s.parent.Has(id) {
		s.parent.Set(id, val)
	} else {
		s.vars[id] = val
	}
}

func (s *Scope) GetFunc(id string) PFunc {
	f, present := s.funs[id]
	if present {
		return f
	} else if s.parent != nil {
		return s.parent.GetFunc(id)
	}
	return nil
}

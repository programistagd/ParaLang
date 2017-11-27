package vm

type Scope map[string]PValue

struct StackState {
    var registers[100]PValue //TODO autoresizing
}

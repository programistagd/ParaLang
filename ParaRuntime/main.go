package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/programistagd/ParaLang/ParaRuntime/vm"
)

func main() {
	fmt.Println("WARNING! This is work in progress.")

	var file = os.Stdin
	if len(os.Args) > 1 {
		if !strings.HasPrefix(os.Args[1], "-") {
			f, err := os.Open(os.Args[1])
			defer f.Close()
			if err != nil {
				panic(err)
			}
			file = f
		}
	}

	reader := bufio.NewReader(file)
	ops := make([]string, 0)

	for {
		line, err := reader.ReadString('\n')
		line = strings.TrimSpace(line)

		if err != nil {
			break
		}

		ops = append(ops, line)
		fmt.Println(line)
	}

	scope := vm.ParseFile(ops)
	_ = scope //TODO CALL main
}

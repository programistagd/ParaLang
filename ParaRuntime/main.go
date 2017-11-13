package main

import (
   "bufio"
   "os"
   "strings"
   "fmt"
)

func main() {
   var file = os.Stdin;
   if (len(os.Args) > 1) {
      if (!strings.HasPrefix(os.Args[1], "-")) {
         f, err := os.Open(os.Args[1])
         defer f.Close()
         if err != nil {
            panic(err)
         }
         file = f
      }
   }

   reader := bufio.NewReader(file)

    for {
        line, err := reader.ReadString('\n')
        line = strings.TrimSpace(line)

        if err != nil {
            break
        }

        fmt.Println(line)
    }
}

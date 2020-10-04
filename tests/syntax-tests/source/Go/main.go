// The hugeparam command identifies by-value parameters that are larger than n bytes.
//
// Example:
//	$ ./hugeparams encoding/xml
package main

import (
	"flag"
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"log"

	"golang.org/x/tools/go/loader"
)

//!+
var bytesFlag = flag.Int("bytes", 48, "maximum parameter size in bytes")

var sizeof = (&types.StdSizes{8, 8}).Sizeof // the sizeof function

func PrintHugeParams(fset *token.FileSet, info *types.Info, files []*ast.File) {
	checkTuple := func(descr string, tuple *types.Tuple) {
		for i := 0; i < tuple.Len(); i++ {
			v := tuple.At(i)
			if sz := sizeof(v.Type()); sz > int64(*bytesFlag) {
				fmt.Printf("%s: %q %s: %s = %d bytes\n",
					fset.Position(v.Pos()),
					v.Name(), descr, v.Type(), sz)
			}
		}
	}
	checkSig := func(sig *types.Signature) {
		checkTuple("parameter", sig.Params())
		checkTuple("result", sig.Results())
	}
	for _, file := range files {
		ast.Inspect(file, func(n ast.Node) bool {
			switch n := n.(type) {
			case *ast.FuncDecl:
				checkSig(info.Defs[n.Name].Type().(*types.Signature))
			case *ast.FuncLit:
				checkSig(info.Types[n.Type].Type.(*types.Signature))
			}
			return true
		})
	}
}

//!-

func main() {
	flag.Parse()

	// The loader loads a complete Go program from source code.
	var conf loader.Config
	_, err := conf.FromArgs(flag.Args(), false)
	if err != nil {
		log.Fatal(err) // command syntax error
	}
	lprog, err := conf.Load()
	if err != nil {
		log.Fatal(err) // load error
	}

	for _, info := range lprog.InitialPackages() {
		PrintHugeParams(lprog.Fset, &info.Info, info.Files)
	}
}

/*
//!+output
% ./hugeparam encoding/xml
/go/src/encoding/xml/marshal.go:167:50: "start" parameter: encoding/xml.StartElement = 56 bytes
/go/src/encoding/xml/marshal.go:734:97: "" result: encoding/xml.StartElement = 56 bytes
/go/src/encoding/xml/marshal.go:761:51: "start" parameter: encoding/xml.StartElement = 56 bytes
/go/src/encoding/xml/marshal.go:781:68: "start" parameter: encoding/xml.StartElement = 56 bytes
/go/src/encoding/xml/xml.go:72:30: "" result: encoding/xml.StartElement = 56 bytes
//!-output
*/

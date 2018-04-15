# File ordering algorithm documentation

Ordering of files alg. is splitted into 3 parts:

1. Collecting symbols from AST
2. From symbols, find dependencies between files
3. Construct oriented graph from dependencies and solve topological order on it

## Collecting symbols from AST

For each file, from untyped tree we collect three types of symbols:
* symbol definition, fully qualified (let bindings, type declaration, DU case constructors, record fields, members) 
* symbol usage
* open declarations, including "implicit" ones from module declaration and namespace declaration

Symbol usage and open declarations are grouped together, so for each usage we have list of valid opens for that symbol.

We divide symbol into these groups:
* Identificator
* Record field
* Type
* Pattern

For collecting, `AstTraverse` code was copied from https://github.com/fsharp/FSharp.Compiler.Service/blob/13ecd8d4d080465bce4f49de72e4c13c6005e842/src/fsharp/vs/ServiceParseTreeWalk.fs and altered to support going through the whole tree. 
This allows us to traverse AST and cherry-pick only certain type of nodes.

Symbols and opens are represented as simple strings.

### Current state
* members are not supported

## From symbols, find dependencies between files

For each symbol usage we try to find corresponding definition. 
We do it by combining symbol with all open declarations, and compare it with definitions.
Because identifier can be partially qualified, we must detect common parts in the end of `open` and the start of indentifier,
so `open A.B.C` + `C.D` -> `A.B.C.D`.

From this we get list of file dependencies `(file1, file2, symbols)`, where `file2` uses `symbols` defined in `file1`.

Actual implementation uses Map with the last part of the qualified identifier of symbol for performance.

## Construct oriented graph from dependencies and solve topological order on it

Correct ordering of files is such that for each dependency `file1 -> file2`, `file1` is before `file2`.
This directly translates to Topological order problem in oriented graphs: https://en.wikipedia.org/wiki/Topological_sort

Kahn's algorithm mentioned on wiki is implemented with modifications to find ordering close to minimal number of *move up/down* operations (switching order of two neighbour elements):

In each cycle, we add only one node to resulting ordered list; from set of nodes with no incoming edge, we select the one where following outgoing edges leads to the smallest position in original order.

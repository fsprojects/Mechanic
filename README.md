# AProjectHasNoName

## Project idea

The F# compiler needs to get files passed in correct order for compilation, so that everything is declared before usage. 
There are some different opinions if this should be seen as a feature or as a limitation. This project won't settle this dispute, but it wants to make it easier for F# developers to maintain that order in fsproj files.

The idea is to create a dotnet core tool that uses the FSharp.Compiler.Service to find a valid file order. Since there may exist more than one valid order, the tool will write the order into the F# project file (the fsproj file). The user will then commit the fsproj into version control and build servers will use exactly the one committed version. In contrast to multi-pass compilers this will remain a step that is done explicitly by the programmer and there always needs to be at least one valid order. In this sense the tool and the fsproj are very similar to paket and the paket.lock file. 



## Tasks 

- [] find a name

## Maintainers

- @forki - Mentoring

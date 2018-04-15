[![Join the chat at https://gitter.im/fsprojects/Mechanic](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/fsprojects/Mechanic?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# Mechanic
<img src="logo/mechanic.png" height="150">

## Project idea

The F# compiler needs to get files passed in correct order for compilation, so that everything is declared before usage. 
There are some different opinions if this should be seen as a feature or as a limitation. This project won't settle this dispute, but it wants to make it easier for F# developers to maintain that order in fsproj files.

The idea is to create a dotnet core tool that uses the FSharp.Compiler.Service to find a valid file order. Since there may exist more than one valid order, the tool will write the order into the F# project file (the fsproj file). The user will then commit the fsproj into version control and build servers will use exactly the one committed version. In contrast to multi-pass compilers this will remain a step that is done explicitly by the programmer and there always needs to be at least one valid order. In this sense the tool and the fsproj are very similar to paket and the paket.lock file. 

In addition to creating a standalone dotnet core CLI tool, we would probably also want to create a standalone library that could be integrated into editors. This would make integration a little simpler and could also take advantage of existing FSharp.Compiler.Service caches. In this case, we would probably want to take an exisiting FSharpChecker instance as a parameter.

### Algorithm

This depends a lot on how much information we get from the FSharp.Compiler.Service. But the following properties should hold:

* If the fsproj already contains a valid order then we don't touch it 
* If we need to change the order then we try to keep the original order as stable as possible.
* If we there are multiple possible orders then we take the first one and exit 
* There may be heuristics needed to prefer certain parts of the search tree. We may try to make it opionated and force good practices
* Files that have similar names should be glued together if possible. 
* Files in the same folder should be hold together if possible. 
* If we can't find a valid order we report an error text that is easy to understand for the programmer 
* If we can't find a valid order we may suggest how to fix the issue 

Algorithm discussion at [#10](https://github.com/fsprojects/AProjectHasNoName/issues/10)

Current algorithm documentation [here](OrderingAlg.md).

## Commands

In real world editing scenarios we will hardly ever need to reorder the whole file list. 
Usually programmers already have a working state and perform changes to it. These changes may introduce small ordering issues or even circular dependencies and the tool should help to resolve these. The following commands seem to be useful:

* Reorder everything based on the fsproj
* Reorder everything based on all files in the folder structure
* Move file up as far as possible
* Move file down as far as possible
* Move file somewhere below file Y

## Getting started

In order to test the tool you can clone the repo and run the following command:

     dotnet run -p src/Mechanic.CommandLine [path/to/a/projectfile.fsproj]

Advanced usage:
    
    USAGE: [--help] [--pattern <string> <string>] [--dry-run] [--log-ast-tree] [--log-collected-symbols] [--log-file-dependencies]
              [--log-file-dependencies-with-symbols] [<string>]

    <string>              Project file.

    OPTIONS:

        --pattern <string> <string>
                            Alternative to project file - specify directory and wildcard pattern. Only print out resulting order.
        --dry-run             Don't update project file.
        --shuffle-test        Do extensive testing of the correctness of Mechanic on given project.
                              Tries varoius order of source files and check Mechanic result by compiler.
        --log-ast-tree        Print out AST tree for each source file from project.
        --log-collected-symbols
                            Print out collected symbols for each source file from project.
        --log-file-dependencies
                            Print out file dependencies in project.
        --log-file-dependencies-with-symbols
                            Print out file dependencies (with depended symbols) in project.

## Tools

The following toolchain is implemented to get us started:

* [Paket][paket] for dependency management 
* [FAKE][fake] as build script
* [dotnet cli][dotnet]
* [FsCheck][fscheck] and [expecto][expecto] for unit testing

[paket]: https://fsprojects.github.io/Paket/
[fake]: https://fake.build/
[dotnet]: https://github.com/dotnet/cli/
[fscheck]: https://fscheck.github.io/FsCheck/
[expecto]: https://github.com/haf/expecto/

To build the project and run the tests, simply run one of the build scripts,
depending on your environment; `build.ps1`, `build.sh` (with the `--use-mono` flag if under non-Windows)
or `build.cmd`.

In the future, we might also want to use e.g.

* [Argu](https://github.com/fsprojects/Argu) for command line parsing

## Tasks 

- find a name (see [#12](https://github.com/fsprojects/AProjectHasNoName/issues/12)) 
- extend this readme
- split the work 
- setup project structure with CI and tests
- write some code
- choose a license
- testing, testing, testing
- integration into Ionide (see [ionide-mechanic](https://github.com/ionide/ionide-mechanic))
- integration into VS

## Maintainers

This project will be done mostly by newcomers to the F# OSS ecosystem. This is an explicit decision in order to get more people involved. Long time members of the F# OSS sphere will mentor and try to help if things get stuck. 
 
- @tlycken - Infrastructure/Build
- @jindraivanek - Topological sorting
- @donopj2 - Project file manipulation
- @forki - Mentoring
- @Krzysztof-Cieslak - Mentoring for FCS questions and Ionide integration

## What about you?

If you are interested in joining this project, then welcome! We are really interested in bringing new people into the F# OSS ecosystem. Don't be shy! One way to get started is to take a look at the list of issues to see if there is anything there you feel like you'd like to contribute with. There's a couple of ongoing discussions on how to structure the library, and especially #10 has some details on where the code base might be heading.

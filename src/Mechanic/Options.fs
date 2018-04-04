namespace Mechanic

module Options =
    type LogOutput = {
        AstTree : bool
        CollectedSymbols : bool
        FileDependencies : bool
        FileDependenciesWithSymbols : bool
    }

    module LogOutput =
        let Default = { 
            AstTree = false
            CollectedSymbols = false
            FileDependencies = false
            FileDependenciesWithSymbols = false
        }

open Options

type Options = {
    LogOutput : LogOutput
}

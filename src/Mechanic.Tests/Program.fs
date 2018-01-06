module Program = 

    open Expecto

    let [<EntryPoint>] main args = 
        runTestsInAssembly defaultConfig args

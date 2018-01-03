module Tests

open FsCheck.Xunit
open Swensen.Unquote
open FSharp.Project.FileOrderer

[<Property>]
let ``Say says hello to whomever we greet`` greetee =
    let greeting = Say.hello greetee
    let expected = "Hello " + greetee
    greeting =! expected

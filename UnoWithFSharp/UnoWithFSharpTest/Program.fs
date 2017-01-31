open Expecto
open Tests

[<EntryPoint>]
let main argv = 
    runTests defaultConfig tests |> ignore
    0 // return an integer exit code

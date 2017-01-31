module Tests

open Game
open Expecto



[<Tests>]
let tests =
    testList "samples"  [
        testCase "1 should be 1" <| fun _ ->
            Expect.equal 1 1 ""
        testCase "universe exists" <| fun _ ->
            let result =
                Game.decide 
                    (StartGame { Players = 3; FirstCard = Digit(Three, Red)}) 
                    InitialState
            Expect.equal 
                (Ok [ GameStarted { Players = 3; FirstCard = Digit(Three, Red)} ] )
                result
                "Game should be started"        
                
        testCase "universe exists" <| fun _ ->
            let result =
                Game.decide 
                    (StartGame { Players = 3; FirstCard = Digit(Three, Red)}) 
                    InitialState
            Expect.equal 
                (Ok [ GameStarted { Players = 3; FirstCard = Digit(Three, Red)} ] )
                result
                "Game should be started"
    ]
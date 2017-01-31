module Tests

open Game
open Expecto



[<Tests>]
let tests =
    testList "samples"  [
        testCase "1 should be 1" <| fun _ ->
            Expect.equal 1 1 ""
        testCase "Game should start" <| fun _ ->
            let result =
                Game.decide 
                    (StartGame { Players = 3; FirstCard = Digit(Three, Red)}) 
                    InitialState
            Expect.equal 
                (Ok [ GameStarted { Players = 3; FirstCard = Digit(Three, Red)} ] )
                result
                "Game should be started"        
                
        testCase "Cannot start game twice" <| fun _ ->
            let firstGame = GameStarted { Players = 2; FirstCard = Digit(Three, Red)}
            let state = evolve InitialState firstGame 
            let result =
                Game.decide 
                    (StartGame { Players = 3; FirstCard = Digit(Three, Red)}) 
                    state
            Expect.equal 
                (Failure "Game already started" )
                result
                "Game should be started"
    ]
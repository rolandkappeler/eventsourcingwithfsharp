module Tests

open Game
open Expecto

let (=>) events command= 
    events
    |> List.fold evolve InitialState
    |> decide command

let (==) result expected = 
    Expect.equal result (Ok expected) "should be equal to expected ok"

let (=!) result expected = 
    Expect.equal result (Failure expected) "should be equal to expected error"



[<Tests>]
let tests =
    testList "samples"  [
        testCase "1 should be 1" <| fun _ ->
            Expect.equal 1 1 ""

        testCase "Game should start" <| fun _ ->
            [] 
            => StartGame { Players = 3; FirstCard = Digit(Three, Red)}
            == [ GameStarted { Players = 3; FirstCard = Digit(Three, Red)} ] 
     
                
        testCase "Cannot start game twice" <| fun _ ->
            [GameStarted { Players = 2; FirstCard = Digit(Three, Red)}]
            => StartGame { Players = 3; FirstCard = Digit(Three, Red)}
            =! GameAlreadyStarted


        testCase "Playing the same color should be ok" <| fun _ ->
            [GameStarted { Players = 2; FirstCard = Digit(Three, Red)}]
            => PlayCard (Digit(Three, Red),1)
            == [CardPlayed (Digit(Three, Red),1)]
                        
        testCase "Playing the wrong color but the same digit should be ok" <| fun _ ->
            [GameStarted { Players = 2; FirstCard = Digit(Three, Red)}]
            => PlayCard (Digit(Three, Blue),1)
            == [CardPlayed (Digit(Three, Blue),1)]

        testCase "Playing the wrong color and different digit should be punished" <| fun _ ->
            [GameStarted { Players = 2; FirstCard = Digit(Three, Red)}]
            => PlayCard (Digit(Two, Blue),1)
            == [WrongCardPlayed]
            
        testCase "Player playing at his turn" <| fun _ ->
            [GameStarted { Players = 2; FirstCard = Digit(Three, Red)};
             CardPlayed (Digit(Three, Blue),1)]
            => PlayCard (Digit(Two, Blue),2)
            ==  [CardPlayed (Digit(Two, Blue),2)]

        testCase "Player playing not at his turn should be punished" <| fun _ ->
            [GameStarted { Players = 2; FirstCard = Digit(Three, Red)};
             CardPlayed (Digit(Three, Blue),1)]
            => PlayCard (Digit(Two, Blue),1)
            == [PlayerDidNotWaitForHisTurn]

    ]
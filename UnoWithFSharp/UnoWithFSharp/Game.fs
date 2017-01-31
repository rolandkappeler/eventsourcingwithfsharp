[<AutoOpen>]
module Game

open Model

type Command = | StartGame of StartGame

and StartGame = {
    Players: int
    FirstCard : Card
}

type Event = | GameStarted of GameStarted

and GameStarted = {
    Players : int
    FirstCard : Card
}

type State =
    | InitialState
    | Started

type Result <'A> =
    | Ok of 'A
    | Failure of Error
and Error = 
    | GameAlreadyStarted

let decide (command:Command) (state:State) =
    match state, command with
    | InitialState, StartGame game -> Ok [ GameStarted { Players = game.Players; FirstCard = game.FirstCard } ]
    | Started, StartGame _ -> Failure GameAlreadyStarted

let evolve (state:State) (event:Event) : State =
    match event with
    | GameStarted e -> Started
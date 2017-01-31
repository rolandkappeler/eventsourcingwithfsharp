[<AutoOpen>]
module Game

open Model

type Command = 
    | StartGame of StartGame
    | PlayCard of Card

and StartGame = {
    Players: int
    FirstCard : Card
}

type Event = 
    | GameStarted of GameStarted
    | CardPlayed of Card
    | WrongCardPlayed

and GameStarted = {
    Players : int
    FirstCard : Card
}

type State =
    | InitialState
    | Started of Card
    | Played of Card

type Result <'A> =
    | Ok of 'A
    | Failure of Error
and Error = 
    | GameAlreadyStarted
    | CannotPlayThisCardNow

let (|SameValue|_|) =
    function
    | Digit(d1,_), Digit(d2,_) when d1 = d2 -> Some()
    | _ -> None

let (|SameColor|_|) =
    function
    | Digit(_, c1), Digit(_,c2) when c1 = c2 -> Some()
    | _ -> None

let decide (command:Command) (state:State) =
    match state, command with
    | InitialState, StartGame game -> Ok [ GameStarted { Players = game.Players; FirstCard = game.FirstCard } ]
    | Started topCard, PlayCard card -> 
        match topCard, card with
        | SameColor | SameValue -> Ok [ CardPlayed card]
        | _ -> Ok [ WrongCardPlayed ]
    | Started _, StartGame _ -> Failure GameAlreadyStarted

let evolve (state:State) (event:Event) : State =
    match event with
    | GameStarted e -> Started  e.FirstCard
    | CardPlayed c -> Played c
[<AutoOpen>]
module Game

open Model

type Command = 
    | StartGame of StartGame
    | PlayCard of Card * int

and StartGame = {
    Players: int
    FirstCard : Card
}

type Event = 
    | GameStarted of GameStarted
    | CardPlayed of Card * int
    | WrongCardPlayed
    | PlayerDidNotWaitForHisTurn

and GameStarted = {
    Players : int
    FirstCard : Card
}

type State =
    | InitialState
    | Played of Card * int

type Result <'A> =
    | Ok of 'A
    | Failure of Error
and Error = 
    | GameAlreadyStarted
    | CannotPlayThisCardNow

let (|SameValue|_|) =
    function
    | (Digit(d1,_),_), (Digit(d2,_),player) when d1 = d2 -> Some()
    | _ -> None

let (|SameColor|_|) =
    function
    | (Digit(_, c1),_), (Digit(_,c2), player) when c1 = c2 -> Some()
    | _ -> None
let (|NextPlayer|_|) =
    function
    | (_,previousPlayer), (_, player) when previousPlayer+1 = player -> Some()
    | _ -> None

let decide (command:Command) (state:State) =
    match state, command with
    | InitialState, StartGame game -> Ok [ GameStarted { Players = game.Players; FirstCard = game.FirstCard } ]
    | Played (topCard, lastPlayer), PlayCard (card,player) -> 
        match (topCard, player), (card, player) with
        | (SameColor | SameValue) & NextPlayer -> Ok [ CardPlayed (card,player) ]

        | _ -> Ok [ WrongCardPlayed ]
    | Played _, StartGame _ -> Failure GameAlreadyStarted


let evolve (state:State) (event:Event) : State =
    match event with
    | GameStarted e -> Played  (e.FirstCard, 1)
    | CardPlayed (c, p) -> Played (c,p)
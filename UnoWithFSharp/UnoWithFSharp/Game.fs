﻿[<AutoOpen>]
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

let decide (command:Command) (state:State) =
    match state, command with
    | InitialState, StartGame game -> Ok [ GameStarted { Players = game.Players; FirstCard = game.FirstCard } ]
    | Started c, PlayCard card -> 
        if colorAreTheSame then
            Ok [ CardPlayed card]
        else Failure "NO!"
    | Started _, StartGame _ -> Failure GameAlreadyStarted

let evolve (state:State) (event:Event) : State =
    match event with
    | GameStarted e -> Started  e.FirstCard
    | CardPlayed c -> Played c
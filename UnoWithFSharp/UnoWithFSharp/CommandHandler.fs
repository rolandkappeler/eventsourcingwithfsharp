﻿module CommandHandler

open Game
open EventStore.ClientAPI
open EventStore
open EventStore.ClientAPI.SystemData
open System

type Slice<'t> =
    | Done of 't list * int
    | Continue of 't list * int

type Read<'t> = string -> int -> Slice<'t> Async

let handler (read:Read<Event>) (write) (stream:string) (cmd:Command) =
    let rec load state version =
        async {
            let! slice = read stream version
            match slice with
            | Done (events, finalVersionNumber) -> 
                let finalState = events |> List.fold evolve state
                return finalState, finalVersionNumber            
            | Continue (events, versionNumber) ->
                let intermediateState = events |> List.fold evolve state
                return! load intermediateState versionNumber
        }
    async {
        let! game, latestVersion = load InitialState 0
        match decide cmd game with
        | Ok newEvents -> 
            do! write stream latestVersion newEvents 
            return Ok()
        | Failure error -> return Failure error
    }

module EventStore =
    open EventStore.ClientAPI
    open Serialisation

    let read(store:IEventStoreConnection) stream version =
        async {
            let! slice = 
                store.ReadStreamEventsForwardAsync(stream, version, 100, true)
                |> Async.AwaitTask
            
            let events =
                slice.Events
                |> Array.toList
                |> List.collect (fun event -> 
                    let data = System.Text.Encoding.UTF8.GetString(event.Event.Data)                    
                    let eventType = event.Event.EventType
                    Serialisation.GameEvents.deserialize(eventType, data)
                )

            if slice.IsEndOfStream then
                return Done(events, slice.LastEventNumber)
            else
                return Continue(events, slice.NextEventNumber)
        }

    let append (store:IEventStoreConnection) stream expectedVersion events =
        async {
            let eventData =
                events
                    |> List.map(fun event -> 
                        let eventType, data = Serialisation.GameEvents.serialize event
                        EventData(
                            System.Guid.NewGuid(),
                            eventType,
                            true,
                            System.Text.Encoding.UTF8.GetBytes(data),
                            null))

            let! result = 
                store.AppendToStreamAsync(stream, expectedVersion, eventData)
                |> Async.AwaitTask
            return()
        }


type Agent<'t> = MailboxProcessor<'t>


let game stream read append = 
    Agent.Start(fun mailbox-> 
     
        let rec loop state version = 
            async{
                let! command= mailbox.Receive()
                let result = decide command state
                let newState = match result with
                | Ok events ->  
                    events
                    |> List.fold evolve state                
                | _ -> state


                
 
               // newVersion = append
                // compute new state
                return! loop newState

            }
        async {
            let! state,version = read stream InitialState 0
            return! loop state version
        }
     )





let game1 = game "Game-1" read append
game1.Post (StartGame {Players=5; FirstCard=Digit(Five,Red)})

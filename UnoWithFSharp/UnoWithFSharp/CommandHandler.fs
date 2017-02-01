module CommandHandler

open Game
open EventStore.ClientAPI

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
            do! write stream newEvents latestVersion
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
                |> Array.map (fun event -> 
                    let data = System.Text.Encoding.UTF8.GetString(event.Event.Data)                    
                    let eventType = event.Event.EventType
                    Serialisation.GameEvents.deserialize(eventType, data)
                )
                |> Array.toList

            if slice.IsEndOfStream then
                return Done(events, slice.LastEventNumber)
            else
                return Continue(events, slice.NextEventNumber)
        }
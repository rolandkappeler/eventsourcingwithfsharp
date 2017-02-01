module CommandHandler

open Game

type Slice<'t> =
    | Done of 't list * int
    | Continue of 't list * int

type Read<'t> = string -> int -> Slice<'t>

let handler (read:Read<Event>) (stream:string) (cmd:Command) =
    let rec load state version =
        async {
            let! slice = read stream version
            match slice with
            | Done (events, finalVersionNumber) -> 
                let finalState = events |> List.fold evolve state
                finalState, finalVersionNumber            
            | Continue (events, versionNumber) ->
                let intermediateState = events |> List.fold evolve state
                load intermediateState versionNumber
        }
    let state, version = load InitialState 0
    let newEvents = decide cmd state

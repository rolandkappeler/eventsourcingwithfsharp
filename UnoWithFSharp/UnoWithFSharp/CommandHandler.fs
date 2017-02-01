module CommandHandler

open Game

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
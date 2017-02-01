// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open CommandHandler
open EventStore
open EventStore.ClientAPI

[<EntryPoint>]
let main argv = 
    let connection = System.Uri("tcp://admin:changeit@localhost:2112")
    let eventStoreConnection = EventStore.ClientAPI.EventStoreConnection.Create(connection)
    let theRead = EventStore.read eventStoreConnection 
    handler (read eventStoreConnection "whatever" 0) () "whatever" StartGame


    printfn "%A" argv
    0 // return an integer exit code

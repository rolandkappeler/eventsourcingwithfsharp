module Game

type Command = | DoSomething

type Event = | SomeThingHappened

type State = | InitialState

let decide command state = []
//    match command with
//    | DoSomething -> seq [ SomeThingHappened ]

let evolve (state:State) (event:Event) : State =
    state
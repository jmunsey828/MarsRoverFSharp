
// State monad 'S' 
type S<'State,'Value> = S of ('State -> 'Value * 'State)

let runS (S f) state = f state 

let returnS x =
    let run state =
        x, state 
    S run 

let bindS f xS =
    let run state =
        let x, newState = runS xS state 
        runS (f x) newState
    S run 

type StateBuilder()=
    member this.Return(x)= returnS x
    member this.Bind(xS, f) = bindS f xS 

let state = new StateBuilder()

let getS =
    let run state =
        (state, state)
    S run

let putS newState =
    let run state =
        ((), newState)
    S run


// Types
type Direction = 
    | North
    | East 
    | South 
    | West 


type RoverState = { 
    direction: Direction
    x: int
    y: int
    }

// Rover Strategy
type IRoverStrategy =
    abstract member turnLeft: S<RoverState, unit>
    abstract member turnRight: S<RoverState, unit>
    abstract member moveForward: S<RoverState, unit>
    abstract member moveBackward: S<RoverState, unit>

type NorthStrategy() =
    interface IRoverStrategy with 
        member this.turnLeft = state {
            let! s = getS
            do! putS {s with direction = West}
            printfn "\nTurn Left: Point West"
            return ()
            }
        member this.turnRight = state {
            let! s = getS
            do! putS {s with direction = East}
            printfn "\nTurn Right: Point East"
            return()
            }
        member this.moveForward = state {
            let! s= getS
            do! putS {s with y= s.y+1}
            printfn "\nMove Forward to (%i,%i)" s.x (s.y+1)
            return ()
            }
        member this.moveBackward = state {
            let! s = getS 
            do! putS {s with y=s.y-1}
            printfn "\nMove Backward to (%i,%i)" s.x (s.y-1)
            return()
            }
            
type SouthStrategy() =
    interface IRoverStrategy with 
        member this.turnLeft = state {
            let! s = getS
            do! putS {s with direction = East}
            printfn "\nTurn Left: Point East"
            return ()
            }
        member this.turnRight = state {
            let! s = getS
            do! putS {s with direction = West}
            printfn "\nTurn Right: Point West"
            return()
            }
        member this.moveForward = state {
            let! s= getS
            do! putS {s with y= s.y-1}
            printfn "\nMove Forward to (%i,%i)" s.x (s.y-1)
            return ()
            }
        member this.moveBackward = state {
            let! s = getS 
            do! putS {s with y=s.y+1}
            printfn "\nMove Backward to (%i,%i)" s.x (s.y+1)
            return()
            }

type EastStrategy() =
    interface IRoverStrategy with 
        member this.turnLeft = state {
            let! s = getS
            do! putS {s with direction = North}
            printfn "\nTurn Left: Point North"
            return ()
            }
        member this.turnRight = state {
            let! s = getS
            do! putS {s with direction = South}
            printfn "\nTurn Right: Point South"
            return()
            }
        member this.moveForward = state {
            let! s= getS
            do! putS {s with x= s.x+1}
            printfn "\nMove Forward to  (%i,%i)" (s.x+1) s.y
            return ()
            }
        member this.moveBackward = state {
            let! s = getS 
            do! putS {s with x=s.x-1}
            printfn "\nMove Backward to  (%i,%i)" (s.x-1) s.y
            return()
            }

type WestStrategy() =
    interface IRoverStrategy with 
        member this.turnLeft = state {
            let! s = getS
            do! putS {s with direction = South}
            printfn "\nTurn Left: Point South"
            return ()
            }
        member this.turnRight = state {
            let! s = getS
            do! putS {s with direction = North}
            printfn "\nTurn Right: Point North"
            return ()
            }
        member this.moveForward = state {
            let! s= getS
            do! putS {s with x= s.x-1}
            printfn "\nMove Forward to  (%i,%i)" (s.x-1) s.y
            return ()
            }
        member this.moveBackward = state {
            let! s = getS 
            do! putS {s with x=s.x+1}
            printfn "\nMove Backward to  (%i,%i)" (s.x+1) s.y
            return()
            }

let getStrategy = state {
    let! s = getS 
    return 
        match s.direction with
        | North -> new NorthStrategy() :> IRoverStrategy
        | South -> new SouthStrategy() :> IRoverStrategy
        | East -> new EastStrategy() :> IRoverStrategy
        | West -> new WestStrategy() :> IRoverStrategy
    }

[<EntryPoint>]
let main argv =
    
    let moveForward = state {
        let! strat = getStrategy 
        do! strat.moveForward
        return()
        }

    let moveBackward = state {
        let! strat = getStrategy 
        do! strat.moveBackward
        return ()
        }

    let turnLeft = state {
        let! strat = getStrategy 
        do! strat.turnLeft
        return ()
        }

    let turnRight = state {
        let! strat = getStrategy 
        do! strat.turnRight
        return ()
        }

    let testPath = state {
        do! moveForward 
        do! turnLeft
        do! moveForward 
        do! moveForward  
        do! turnLeft
        do! moveForward  
        do! turnRight
        do! moveBackward
        return ()
        }

    
    let startLocation = {direction=North; x=0; y=0}
    printfn "Starting location: %A" startLocation

    let endLocation = runS testPath startLocation
    printfn "/nEnding location: %A/n" endLocation
     

    0 // return an integer exit code

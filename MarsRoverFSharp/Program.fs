
type S<'Value,'State> = S of ('State -> 'Value * 'State)

let runS (S f) state = f state 

let bindS f xS =
    let run state =
        let (x, s) = runS xS state
        runS (f x) s
    S run 

let returnS x =
    let run state =
        (x, state)
    S run

let getS =
    let run state =
        state, state
    S run

let putS newState =
    let run _ =
        (), newState
    S run
    

type StateBuilder()=
    member this.Bind(xS, f) = bindS f xS 
    member this.Return(x) = returnS x
    member this.Zero(x) = x 

let state = new StateBuilder()


type Direction =
    | North
    | South
    | East 
    | West

type Rover = {X:int; Y:int; direction: Direction}

type RoverOutput =
    | TurningLeft
    | TurningRight
    | MovingForward
    | MovingBackward


let turnRightS =
    state {
        let! rover = getS
        let newDirection =
            match rover.direction with 
            | North -> East 
            | East -> South
            | South -> West
            | West -> North
        do! putS {rover with direction = newDirection} 
        
        return TurningRight
        }

let turnLeftS =
    state {
        let! rover = getS
        let newDirection = 
            match rover.direction with
            | North -> West
            | West -> South
            | South -> East
            | East -> North 
        do! putS {rover with direction = newDirection}

        return TurningLeft
        }

let moveForwardS =
    state {
        let! rover = getS 
        let newRover = 
            match rover.direction with
            | North -> {rover with Y = rover.Y + 1}
            | South -> {rover with Y = rover.Y - 1}
            | East -> {rover with X = rover.X + 1}
            | West -> {rover with X = rover.X - 1}
        do! putS newRover

        return MovingForward
        }

let moveBackwardS =
    state {
        let! rover = getS
        let newRover =
            match rover.direction with
            | North -> {rover with Y = rover.Y - 1}
            | South -> {rover with Y = rover.Y + 1}
            | East -> {rover with X = rover.X - 1}
            | West -> {rover with X = rover.X + 1}
        do! putS newRover

        return MovingBackward
        }

let perform180M =
    state {
        let! t1 = turnLeftS
        let! t2 = turnLeftS
        return [t1;t2]
        }

let makeCircle =
    state {
        let! t1 = turnRightS
        let! t2 = moveForwardS
        let! t3 = turnLeftS
        let! t4 = moveForwardS
        let! t5 = turnLeftS
        let! t6 = moveForwardS
        let! t7 = turnLeftS
        let! t8 = moveForwardS
        let! t9 = perform180M

        return [t1; t2; t3; t4; t5; t6; t7; t8] @ t9
        }


[<EntryPoint>]
let main argv =
  
    let rover = {X=0; Y=0; direction=North}

    let roverOutput, endingRover = runS makeCircle rover

    roverOutput 
    |> List.map (sprintf "%A")
    |> List.reduce (fun x y -> sprintf "%s; %s" x y)
    |> printfn "%s" 
    
    printfn "%A" endingRover 

    printfn ""
    printfn ""

    0 // return an integer exit code
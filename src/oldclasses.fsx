#r "System.Xml.Linq.dll"
#r "FSharp.Data.dll"

open System
open System.Collections
open FSharp.Data
open FSharp.Data.JsonExtensions

/// <summary>Stores a Simulation History Record</summary>
/// <param name="tick:int">The tick time to store this record for</param>
/// <param name="prey:int">Number of preys</param>
/// <param name="predator:int">Number of predators</param>
/// <returns>type</returns>
type HistoryRecord(tick: int, prey: int, predator: int) =
    member tick = tick
    member prey = prey
    member predator = predator

[<AbstractClass>]
type Animal(simulation, breedTime, x, y) =
    member val simulation = simulation : Simulation
    member val breedTime = breedTime
    member val breedClock = simulation.clockTick with get, set
    member val position = (x, y) with get, set
    member val isDead = false with get, set

    // returnerer liste af hosliggende celler
    member this.adjacent =
        let (x, y) = this.position
        let grid = this.simulation.grid
        let width = Array2D.length1 grid
        let height = Array2D.length2 grid
        let random = Random()
        List.sortWith (fun _ _ -> random.Next(-4, 4))
            (List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < width && y < height)
                [x + 1, y; x, y + 1; x - 1, y; x, y - 1])

    // returnerer enkel hosliggende celle der er tom (hvis der er en, Some, ellers None)
    member this.adjacentEmpty =
        let grid = this.simulation.grid
        List.tryFind (fun (x, y) -> Option.isNone grid.[x, y]) this.adjacent

    // returnerer enkel hosliggende celle der er fyldt  (hvis der er en, Some, ellers None)
    member this.adjacentFilled =
        let grid = this.simulation.grid
        List.tryFind (fun (x, y) -> Option.isSome grid.[x, y]) this.adjacent

    member this.breed =
        let cell = this.adjacentEmpty
        let clockTick = this.simulation.clockTick
        if Option.isSome cell && clockTick >= this.breedTime + this.breedClock then
            this.breedClock <- clockTick
            this.clone cell.Value
            true
        else false

    member this.move =
        let cell = this.adjacentEmpty
        if Option.isSome cell then
            let grid = this.simulation.grid
            let (x, y) = this.position
            let (x', y') = cell.Value
            grid.[x, y] <- None
            grid.[x', y'] <- Some(this)
            this.position <- (x', y')
            true
        else false

    abstract member clone : int * int -> unit
    abstract member simulate : unit

and Prey(simulation, x, y, breedTime) =
    inherit Animal(simulation, breedTime, x, y)

    override this.clone (x, y) =
        this.simulation.addPrey (x, y) this.breedTime // randomize breedTime?

    override this.simulate =
        if this.isDead then printfn "dead prey"
        elif this.breed then printfn "breeding prey"
        elif this.move then printfn "moving prey"

and Predator(simulation, x, y, breedTime, starveTime) =
    inherit Animal(simulation, breedTime, x, y)
    member val starveTime = starveTime
    member val starveClock = simulation.clockTick with get, set

    member this.adjacentPrey =
        let grid = this.simulation.grid
        List.tryFind (fun (x, y) -> Option.isSome grid.[x, y] && grid.[x, y].Value :? Prey) this.adjacent

    member this.feed =
        let cell = this.adjacentPrey
        if Option.isSome cell then
            this.simulation.kill cell.Value
            this.starveClock <- this.simulation.clockTick
            true
        else false

    override this.clone (x, y) =
        this.simulation.addPredator (x, y) this.breedTime this.starveTime // randomize breedTime, starveTime?

    override this.simulate =
        let clockTick = this.simulation.clockTick
        if clockTick >= this.starveTime + this.starveClock then
            this.simulation.kill this.position
            printfn "starving predator"
        elif this.isDead then printfn "dead predator"
        elif this.feed then printfn "feeding predator"
        elif this.breed then printfn "breeding predator"
        elif this.move then printfn "moving predator"

and Simulation(width, height) =
    member val clockTick = 0 with get, set
    member val animals = [] : list<Animal> with get, set
    member val grid = Array2D.create width height (Option<Animal>.None)

    member this.simulate =
        List.iter (fun (animal : Animal) -> animal.simulate) this.animals
        this.animals <- List.filter (fun (animal : Animal) -> not animal.isDead) this.animals
        this.clockTick <- this.clockTick + 1

    member this.kill (x, y) =
        this.grid.[x, y].Value.isDead <- true
        this.grid.[x, y] <- None

    member this.print = // behøves ikke
        let format (cell : Option<Animal>) =
            match cell with
            | None -> " "
            | Some(animal) ->
                match animal with
                | :? Prey -> "O"
                | :? Predator -> "X"
                | _ -> " "
        printfn "%A" (Array2D.mapi (fun x y (animal : Option<Animal>) -> format animal) this.grid)

    member this.addPrey (x, y) breedTime =
        let animal = Prey(this, x, y, breedTime) // randomize breedTime?
        this.animals <- (upcast animal)::this.animals
        this.grid.[x, y] <- Some(upcast animal)

    member this.addPredator (x, y) breedTime starveTime =
        let animal = Predator(this, x, y, breedTime, starveTime) // randomize breedTime, starveTime?
        this.animals <- (upcast animal)::this.animals
        this.grid.[x, y] <- Some(upcast animal)

// EXAMPLE USAGE
//let test = Simulation(12, 12)
//test.addPrey (2, 2) 7
//test.addPrey (6, 6) 9
//test.addPrey (4, 9) 11
//test.addPredator (4, 4) 12 16
//test.addPredator (9, 2) 20 14
//test.print
//
//for i in 1 .. 100 do
//    test.simulate
//    test.print

type Settings(jsonPath:string) =
    let read = JsonValue.Load(jsonPath)
    let checkJson json defaultVal =
        if json <> JsonValue.Null then
            json.AsInteger()
        else
            defaultVal
    member this.width with get() = checkJson read?width 50
    member this.height with get() = checkJson read?height 50
    member this.numberOfPredators with get() =  checkJson read?numberOfPredators 10
    member this.numberOfPreys with get() =  checkJson read?numberOfPreys 10
    member this.starveTime with get() =  checkJson read?starveTime 5
    member this.predatorBreedTime with get() =  checkJson read?predatorBreedTime 5
    member this.preyBreedTime with get() =  checkJson read?preyBreedTime 5
    member this.timeSpan with get() = checkJson read?timeSpan 50


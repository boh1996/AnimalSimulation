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

/// <summary>Animal</summary>
/// <param name="breedTime:int">breedTime:int</param>
/// <param name="position:Position">Current position of the Animal</param>
[<AbstractClass>]
type Animal(simulation, breedTime, x, y) =
    member val simulation = simulation : Simulation
    member val breedTime = breedTime
    member val breedClock = simulation.clockTick with get, set
    member val position = (x, y) with get, set

    // returnerer liste af hosliggende celler
    member this.adjacent =
        let (x, y) = this.position
        let grid = this.simulation.grid
        let width = Array2D.length1 grid
        let height = Array2D.length2 grid
        List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < width && y < height) [x + 1, y; x, y + 1; x - 1, y; x, y - 1]

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

    member this.move =
        // add stuff here
        printfn "move"

    abstract member clone : int * int -> unit
    abstract member simulate : unit

and Prey(simulation, x, y, breedTime) =
    inherit Animal(simulation, breedTime, x, y)

    override this.clone (x, y) =
        this.simulation.addPrey (x, y) this.breedTime // randomize breedTime?

    override this.simulate =
        // add behavior here
        base.breed

and Predator(simulation, x, y, breedTime, starveTime) =
    inherit Animal(simulation, breedTime, x, y)
    member val starveTime = starveTime
    member val starveClock = simulation.clockTick with get, set

    override this.clone (x, y) =
        this.simulation.addPredator (x, y) this.breedTime this.starveTime // randomize breedTime, starveTime?

    override this.simulate =
        // add behavior here
        base.breed

and Simulation(width, height) =
    member val clockTick = 0 with get, set
    member val animals = [] : list<Animal> with get, set
    member val grid = Array2D.create width height (Option<Animal>.None)

    member this.simulate =
        List.iter (fun (animal : Animal) -> animal.simulate) this.animals
        this.clockTick <- this.clockTick + 1

    member this.addPrey (x, y) breedTime =
        let animal = Prey(this, x, y, breedTime) // randomize breedTime?
        this.animals <- (upcast animal)::this.animals
        this.grid.[x, y] <- Some(upcast animal)

    member this.addPredator (x, y) breedTime starveTime =
        let animal = Predator(this, x, y, breedTime, starveTime) // randomize breedTime, starveTime?
        this.animals <- (upcast animal)::this.animals
        this.grid.[x, y] <- Some(upcast animal)

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


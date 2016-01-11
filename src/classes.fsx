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

/// Map position x * y
type Position = int*int

/// <summary>Animal</summary>
/// <param name="breedTime:int">breedTime:int</param>
/// <param name="position:Position">Current position of the Animal</param>
type Animal(simulation : Simulation, x, y, breedTime) =
    let _simulation = simulation
    let _breedTime = breedTime
    let _breedClock = simulation.tick
    let mutable _x = x
    let mutable _y = y

    let clamp (x, y) =
        if x < 0 || y < 0 || x > _simulation.width || y > _simulation.height || (_x = x && _y = y)
        then None
        else Some(x, y)

    member this.simulation =
        _simulation

    member this.breedTime =
        _breedTime

    member this.breedClock =
        _breedClock

    member this.x =
        _x

    member this.y =
        _y

    member this.breed =
        let adjacent = this.adjacent
        let (x, y) = List.head adjacent

        if not (List.isEmpty adjacent) && Option.isNone (_simulation.cell x y) && _simulation.tick >= _breedTime + _breedClock
        then this.clone x y

    member this.adjacent =
        List.choose clamp [_x + 1, _y; _x, _y + 1; _x - 1, _y; _x, _y - 1]

    abstract member clone : int -> int -> unit
    default this.clone x y =
        printfn "this should not even happen"

    abstract member simulate : unit
    default this.simulate =
        this.breed

and Prey(simulation, x, y, breedTime) =
    inherit Animal(simulation, x, y, breedTime)

    override this.clone x y =
        this.simulation.addPrey x y this.breedTime

    override this.simulate =
        base.simulate

and Predator(simulation, x, y, breedTime, starveTime) =
    inherit Animal(simulation, x, y, breedTime)
    let _starveTime = starveTime // IMPLEMENT
    let _starveClock = 0 // IMPLEMENT

    member this.starveTime =
        _starveTime

    member this.starveClock =
        _starveClock

    override this.clone x y =
        this.simulation.addPredator x y this.breedTime this.starveTime

    override this.simulate =
        base.simulate

and Simulation() =
    let mutable _tick = 0
    let mutable _animals = [] : list<Animal>
    let _grid = Array2D.create 5 5 (Option<Animal>.None)

    member this.width =
        Array2D.length1 _grid

    member this.height =
        Array2D.length2 _grid

    member this.cell x y =
        _grid.[x, y]

    member this.tick =
        _tick

    member this.simulate =
        List.iter (fun (animal : Animal) -> animal.simulate) _animals
        _tick <- _tick + 1

    member this.print =
        printfn "%A\n%A\n%A" _tick _animals _grid

    member this.addPrey x y breedTime =
        let animal = Prey(this, x, y, breedTime)
        _animals <- (upcast animal)::_animals
        _grid.[x, y] <- Some(upcast animal)

    member this.addPredator x y breedTime starveTime =
        let animal = Predator(this, x, y, breedTime, starveTime)
        _animals <- (upcast animal)::_animals
        _grid.[x, y] <- Some(upcast animal)

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


#r "System.Xml.Linq.dll"
#r "FSharp.Data.dll"
#r "System.Runtime.Serialization"

open System
open System.Collections
open FSharp.Data
open FSharp.Data.JsonExtensions
open System.IO
open System.Runtime.Serialization.Json
open System.Xml
open System.Text

/// <summary>Stores a Simulation History Record</summary>
/// <param name="tick:int">The tick time to store this record for</param>
/// <param name="prey:int">Number of preys</param>
/// <param name="predator:int">Number of predators</param>
/// <returns>type</returns>
type HistoryRecord(tick:int,prey:int,predator:int) =
  /// <summary>Current tick</summary>
  member this.tick = tick

  /// <summary>Number of preys at this tick</summary>
  member this.prey = prey

  /// <summary>Number of predators at this tick</summary>
  member this.predator = predator

  /// <summary>Converts the HistoryRecord to a JSON string</summary>
  /// <returns>JSON string</returns>
  member this.toJSON() =
    sprintf "{\"tick\":%d,\"prey\":%d,\"predator\":%d}" this.tick this.prey this.predator

/// <summary>A class to hold default values</summary>
/// <param name="jsonPath:string">The file to load the default values from</param>
type Settings(jsonPath:string) =
  let read = JsonValue.Load(__SOURCE_DIRECTORY__  + jsonPath)

  /// <summary>Checks if the json value exists</summary>
  /// <returns>The default value or JSON value</returns>
  let checkJson json defaultVal =
    if json <> JsonValue.Null then
      json.AsInteger()
    else
      defaultVal

  /// <summary>Map width</summary>
  member this.width with get() = checkJson read?width 50

  /// <summary>Map height</summary>
  member this.height with get() = checkJson read?height 50

  /// <summary>Number of predators at the beginning</summary>
  member this.numberOfPredators with get() =  checkJson read?numberOfPredators 10

  /// <summary>Number of preys at the beginning</summary>
  member this.numberOfPreys with get() =  checkJson read?numberOfPreys 10

  /// <summary>starveTime for predators</summary>
  member this.starveTime with get() =  checkJson read?starveTime 5

  /// <summary>Ticks between breeding for predators</summary>
  member this.predatorBreedTime with get() =  checkJson read?predatorBreedTime 5

  /// <summary>Ticks between breeding for preys</summary>
  member this.preyBreedTime with get() =  checkJson read?preyBreedTime 5

  /// <summary>Number of ticks the simulation is running</summary>
  member this.timeSpan with get() = checkJson read?timeSpan 50
  
[<AbstractClass>]
/// <summary>Animal</summary>
/// <param name="simulation">The Simulation object, so grid can be accessed</param>
/// <param name="breedTime">The breeding time for this animal</param>
/// <param name="x">X position the Animal starts at</param>
/// <param name="y">Y position the Animal starts at</param>
/// <returns>Animal</returns>
type Animal(simulation, breedTime, x, y) =
    member val simulation = simulation : Simulation
    member val breedTime = breedTime
    member val breedClock = simulation.clockTick with get, set
    member val position = (x, y) with get, set
    member val isDead = false with get, set

    /// <summary>Returns a list of adjacent cells</summary>
    /// <returns>Array</returns>
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
    /// <summary>Finds empty adjacent cells</summary>
    /// <returns>Array[Option]</returns>
    member this.adjacentEmpty =
        let grid = this.simulation.grid
        List.tryFind (fun (x, y) -> Option.isNone grid.[x, y]) this.adjacent

    /// <summary>Finds adjacent filled cells</summary>
    /// <returns>Array[Option]</returns>
    member this.adjacentFilled =
        let grid = this.simulation.grid
        List.tryFind (fun (x, y) -> Option.isSome grid.[x, y]) this.adjacent

    /// <summary>Checks if this animal is ready to breed, and if - it clones it</summary>
    /// <returns>boolean</returns>
    member this.breed =
        let cell = this.adjacentEmpty
        let clockTick = this.simulation.clockTick
        if Option.isSome cell && clockTick >= this.breedTime + this.breedClock then
            this.breedClock <- clockTick
            this.clone cell.Value
            true
        else false

    /// <summary>If an empty adjacent cell is there, move this animal</summary>
    /// <returns>boolean</returns>
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

/// <summary>Prey</summary>
/// <param name="simulation">The parent simulation object</param>
/// <param name="x">Spawn x coordinate of this Prey</param>
/// <param name="y">Spawn y coordinate of this Prey</param>
/// <param name="breedTime">Breeding time for this Prey</param>
/// <returns>Prey</returns>
and Prey(simulation, x, y, breedTime) =
    inherit Animal(simulation, breedTime, x, y)

    /// <summary>Creates a new Prey at (x, y)</summary>
    /// <param name="x">X-coodinate of the new Prey</param>
    /// <param name="y">Y-coodinate of the new Prey</param>
    override this.clone (x, y) =
        this.simulation.addPrey (x, y)

    /// <summary>Operations to run at each tick</summary>
    /// <returns>unit</returns>
    override this.simulate =
        if this.isDead then ()
        elif this.breed then ()
        elif this.move then ()

/// <summary>Predator</summary>
/// <param name="simulation">The parent simulation object</param>
/// <param name="x">Spawn x coordinate of this Prey</param>
/// <param name="y">Spawn y coordinate of this Prey</param>
/// <param name="breedTime">Breeding time for this Prey</param>
/// <param name="starveTime">Starving time for this Predator</param>
/// <returns>Predator</returns>
and Predator(simulation, x, y, breedTime, starveTime) =
    inherit Animal(simulation, breedTime, x, y)
    member val starveTime = starveTime
    member val starveClock = simulation.clockTick with get, set

    /// <summary>Returns some(Animal) if a Prey can be found in the adjacent cellls</summary>
    /// <returns>Option</returns>
    member this.adjacentPrey =
        let grid = this.simulation.grid
        List.tryFind (fun (x, y) -> Option.isSome grid.[x, y] && grid.[x, y].Value :? Prey) this.adjacent

    /// <summary>Tries to find a prey to eat</summary>
    /// <returns>boolean</returns>
    member this.feed =
        let cell = this.adjacentPrey
        if Option.isSome cell then
            this.simulation.kill cell.Value
            this.starveClock <- this.simulation.clockTick
            true
        else false

    /// <summary>Creates a new Predator at (x, y)</summary>
    /// <param name="x">X-coodinate of the new Predator</param>
    /// <param name="y">Y-coodinate of the new Predator</param>
    override this.clone (x, y) =
        this.simulation.addPredator (x, y)

    /// <summary>Operations to run at each tick</summary>
    /// <returns>unit</returns>
    override this.simulate =
        let clockTick = this.simulation.clockTick
        if clockTick >= this.starveTime + this.starveClock then
          this.simulation.kill this.position
        elif this.isDead then ()
        elif this.feed then ()
        elif this.breed then ()
        elif this.move then ()

/// <summary>Simulation class, controls the simulation and holds grid, clockTick etc</summary>
/// <param name="settings:Settings">Initial settings object</param>
/// <returns>Simulation</returns>
and Simulation(settings:Settings) =
  member this.settings = settings
  member val history = [||] with get, set
  member val clockTick = 0 with get, set
  member val animals = [] : list<Animal> with get, set
  member val grid = Array2D.create settings.width settings.height (Option<Animal>.None)

  /// <summary>Call this function to start the simulation</summary>
  member this.simulate() =
    let mutable json = ""
    for i=0 to this.settings.timeSpan-1 do
      this.clockTick <- i
      if i > 0 then
        List.iter (fun (animal : Animal) -> animal.simulate) this.animals
        this.animals <- List.filter (fun (animal : Animal) -> not animal.isDead) this.animals
      let (pred:int, prey:int) = List.fold ( fun (pd, py) (animal:Animal) -> if animal :? Prey then (pd,py+1) else (pd+1,py))(0,0) this.animals
      let h = new HistoryRecord(this.clockTick, prey, pred)
      this.history <- Array.append this.history [||]
      json <- json + "\t" + h.toJSON()
      if i < this.settings.timeSpan-1 then json <- json + ",\n"
    json <- sprintf "[\n%s\n]" json
    let name = DateTime.Now.ToString()
    System.IO.File.WriteAllText("./output/" + name  + ".json",json)
    System.Console.Clear()
    printfn "History records has been saved in \"output/%s.json\"!" name
    System.Console.ReadKey() |> ignore
    System.Console.Clear()

  /// <summary>Kills the Animal at (x, y)</summary>
  /// <param name="x">X coordinate of the Animal to kill</param>
  /// <param name="y">Y coordinate of the Animal to kill</param>
  member this.kill (x, y) =
    this.grid.[x, y].Value.isDead <- true
    this.grid.[x, y] <- None

  /// <summary>Adds a Prey at (x, y) position</summary>
  /// <param name="x">X coodinate to spawn at</param>
  /// <param name="y">Y-coodinate to spawn at</param>
  member this.addPrey (x, y) =
    let animal = Prey(this, x, y, this.settings.preyBreedTime)
    this.animals <- (upcast animal)::this.animals
    this.grid.[x, y] <- Some(upcast animal)

  /// <summary>Adds a predator at (x, y) position</summary>
  /// <param name="x">X coodinate to spawn at</param>
  /// <param name="y">Y-coodinate to spawn at</param>
  member this.addPredator (x, y) =
    let animal = Predator(this, x, y, this.settings.predatorBreedTime, this.settings.starveTime)
    this.animals <- (upcast animal)::this.animals
    this.grid.[x, y] <- Some(upcast animal)

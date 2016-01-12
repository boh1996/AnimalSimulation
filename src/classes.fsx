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

/// <summary>Type to hold a 2D map position</summary>
type Position = int*int

/// <summary>Animal</summary>
/// <param name="breedTime:int">breedTime:int</param>
/// <param name="position:Position">Current position of the Animal</param>
[<AbstractClass>]
type Animal(breedTime:int,position:Position) =
  let mutable _position,_age=position,0
  member val age = _age with get, set
  member this.position with get() = _position
  member this.breedTime with get() = breedTime
  member val breedClock = 0 with get, set
  abstract member move:Position->unit
  member this.breed() = ()
  member this.tick() =
    this.breedClock <- this.breedClock + 1
    this.age <- this.age + 1
  //abstract member tick:Array2D->unit

type Prey(breedTime: int, position: Position) =
  inherit Animal(breedTime, position)

  override this.move(position: Position) = ()

type Predator(breedTime:int,starveTime:int,position:Position) =
  inherit Animal(breedTime, position)
  let mutable _starveTime = starveTime
  member this.starveTime with get() = _starveTime
  member this.starveClock with get() = 0

  override this.move(position: Position) = ()
  member this.eat() = ()

/// <summary>A class to hold default values</summary>
/// <param name="jsonPath:string">The file to load the default values from</param>
type Settings(jsonPath:string) =
  let read = JsonValue.Load(jsonPath)

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

type Simulation(settings:Settings) =
  member this.settings = settings
  member val map = Array2D.create settings.width settings.height (Option<Animal>.None) with get, set
  member val history = [||] with get, set
  member val animals = [||]
  member val clockTick = 0 with get, set
  member this.simulate() =
    let mutable json = ""
    for i=1 to this.settings.timeSpan do
      this.clockTick <- i
      printfn "%d" i
      let h = new HistoryRecord(this.clockTick, 0, 0)
      this.history <- Array.append this.history [||]
      json <- json + "\t" + h.toJSON()
      if i < this.settings.timeSpan then json <- json + ",\n"
    json <- sprintf "[\n%s\n]" json
    System.IO.File.WriteAllText("./output/" + (DateTime.Now.ToString()) + ".json",json)

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
  member this.tick = tick
  member this.prey = prey
  member this.predator = predator

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
      use ms = new MemoryStream() 
      (new DataContractJsonSerializer(typeof<HistoryRecord>)).WriteObject(ms, h) 
      json <- json + Encoding.Default.GetString(ms.ToArray())
    printfn "%A" json

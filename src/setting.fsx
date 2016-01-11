#r "System.Xml.Linq.dll"
#r "FSharp.Data.dll"

open System
open System.Collections
open FSharp.Data

type Settings(jsonPath:string) =
  let read = JsonValue.Load(jsonPath)

  member this.width with get() = 0
  member this.height with get() = 0
  member this.numberOfPredators with get() = 0
  member this.numberOfPreys with get() = 0
  member this.starveTime with get() = 0
  member this.predatorBreedTime with get() = 0
  member this.preyBreedTime with get() = 0
  member this.timeSpan with get() = 0

let s = new Settings("settings.json")
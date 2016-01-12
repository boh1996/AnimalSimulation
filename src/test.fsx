#r "System.Xml.Linq.dll"
#r "FSharp.Data.dll"
open System
open System.Collections
open FSharp.Data
open FSharp.Data.JsonExtensions

#load "./classes.fsx"
open Classes

let Assert f b str =
    try
        let a = f()
        if a = b then
            printfn "Successful for %s" str
            a = b
        else
            printfn "Got %O expected %O for %s" a b str
            a = b
    with
    | :? System.ArgumentException as e ->
        printfn "%O" e
        false
    | Failure(msg) ->
        printfn "%O" msg
        false
    | _ as x ->
        printfn "%O" x
        false

let filePath = "settings/default.json"
let s = new Settings(filePath)

let tests = []

// Match all tests
//Assert ( fun () -> List.forall ( fun x -> x = true ) tests = true ) true "All tests"
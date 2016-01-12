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


// 
// Settings tests
//
let filePath = "settings/default.json"
let s = new Settings(filePath)

let settingsTest1 = Assert (fun () -> s.width) 12 "Settings load json test 01"

// 
// Simulate tests
//
let simulation = new Simulation(s)

let grid = simulation.grid

// Add prey
simulation.addPrey (2,2)
let t2 = Assert (fun () -> simulation.grid.[2, 2] <> None) true "Add prey test 01"

// Add predator
simulation.addPredator (2,3)
let t3 = Assert (fun () -> simulation.grid.[2, 3] <> None) true "Add predator test 01"

// Find adjacent animals
let adj = simulation.animals.[0].adjacentFilled
let t4 = Assert (fun () -> adj.IsSome) true "Adjacent test 01"

// Kill an Animal
simulation.kill (2, 2)
let t5 = Assert (fun () -> simulation.grid.[2, 2].IsNone) true "Kill test 01"

// Adjacent empty
let adjE = simulation.animals.[0].adjacentEmpty
let t6 = Assert (fun () -> adjE.IsSome ) true "Adjacent empty test 01"

let t7 = Assert (fun () -> simulation.animals.[0].move ) true "Move test 01"

let t8 = Assert (fun () -> simulation.grid.[2, 3].IsNone ) true "Move test 02"

let tests = [
    settingsTest1;
    t2;
    t3;
    t4;
    t5;
    t6;
    t7;
    t8;
]

// Match all tests
Assert ( fun () -> List.forall ( fun x -> x = true ) tests = true ) true "All tests"

open System
open System.Collections

type Animal(simulation : Simulation, x, y, breedTime) =
    let _simulation = simulation
    let _breedTime = breedTime
    let _breedClock = simulation.tick
    let mutable _x = x
    let mutable _y = y

    member this.canBreed =
   

    member this.adjacent =
        [_x + 1, _y; _x, _y + 1; _x - 1, _y; _x, _y - 1]

    abstract member simulate : unit
    default this.simulate =
        printfn "canBreed: %A" (this.canBreed)

and Prey(simulation, x, y, breedTime) =
    inherit Animal(simulation, x, y, breedTime)

    override this.simulate =
        base.simulate

and Predator(simulation, x, y, breedTime, starveTime) =
    inherit Animal(simulation, x, y, breedTime)
    let starveTime = starveTime

    override this.simulate =
        base.simulate

and Simulation() =
    let mutable _tick = 0
    let mutable _animals = [] : list<Animal>
    let _grid = Array2D.create 5 5 (Option<Animal>.None)

    member this.cell x y =
        _grid.[x, y]

    member this.tick =
        _tick   

    member this.addPrey x y breedTime =
        let animal = Prey(this, x, y, breedTime)
        _animals <- (upcast animal)::_animals
        _grid.[x, y] <- Some(upcast animal)

    member this.addPredator x y breedTime starveTime =
        let animal = Predator(this, x, y, breedTime, starveTime)
        _animals <- (upcast animal)::_animals
        _grid.[x, y] <- Some(upcast animal)

    member this.simulate =
        List.iter (fun (animal : Animal) -> animal.simulate) _animals
        _tick <- _tick + 1

    member this.print =
        printfn "%A\n%A\n%A" _tick _animals _grid

let test = Simulation()
test.addPrey 0 0 5
test.print
test.simulate

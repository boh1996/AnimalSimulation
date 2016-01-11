open System.Collections;

/// <summary>Stores a Simulation History Record</summary>
/// <param name="tick:int">The tick time to store this record for</param>
/// <param name="prey:int">Number of preys</param>
/// <param name="predator:int">Number of predators</param>
/// <returns>type</returns>
type HistoryRecord(tick: int, prey: int, predator: int) =
  member tick = tick
  member prey = prey
  member predator = predator

type Position = int*int

/// <summary>Animal</summary>
/// <param name="breedTime:int">breedTime:int</param>
/// <param name="position:Position">Current position of the Animal</param>
[<AbstractClass>]
type Animal(breedTime:int,position:Position) =
  let mutable _position,_age=position,0
  member this.age with get() = _age
  member this.position with get() = _position
  member this.breedTime with get() = breedTime
  member this.breedClock with get() = 0
  abstract member move:Position->unit
  member this.breed() = ()
  abstract member tick:Array2D->unit

type Prey(breedTime: int, position: Position)
  inherit Animal(breedTime, position)

  override this.move(position: Position)

type Predator(breedTime: int, starveTime: int, position: Position)
  inherit Animal(breedTime, position)
  let mutable _starveTime = starveTime
  member this.starveTime with get() = _starveTime
  member this.starveClock with get() = 0

  override this.move(position: Position)
  member this.eat() = ()

type Simulation() =
  member map = Array2D.create width height (Option<Animal>.None)
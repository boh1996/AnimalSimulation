/// <summary>Stores a Simulation History Record</summary>
/// <param name="tick:int">The tick time to store this record for</param>
/// <param name="prey:int">Number of preys</param>
/// <param name="predator:int">Number of predators</param>
/// <returns>type</returns>
type HistoryRecord(tick: int, prey: int, predator: int) =
  member tick = tick
  member prey = prey
  member predator = predator

type Prey(breedTime: int, position: Position)
  inherit Animal(breedTime, position)

  override this.move(position: Position)
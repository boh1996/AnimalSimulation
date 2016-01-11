type Position = int*int

/// <summary></summary>
/// 
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

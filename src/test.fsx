open System.Collections

type Animal() =
    class end

let l = [||]
let a = new Animal()

Array.append l [|a|]
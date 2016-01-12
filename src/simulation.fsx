#load "./classes.fsx"
open Classes

let write (str:string) = System.Console.Write str
let writeln (str:string) = System.Console.WriteLine str
let readln() = System.Console.ReadLine()
let setcursor(x,y) = System.Console.SetCursorPosition(x,y)
let clear() = System.Console.Clear()
let validate_file str = System.IO.File.Exists(str)

let rec main() =
  let mutable input = ""
  System.Threading.Thread.Sleep(50)
  clear()
  while not (validate_file input) do
    writeln "Skriv stien til din settings fil:"
    input <- readln()
    clear()
  let settings = new Settings(input)
  let simulation = new Simulation(settings)
  simulation.addPrey (2, 2) 7
  simulation.addPrey (6, 6) 9
  simulation.addPrey (4, 9) 11
  simulation.addPredator (4, 4) 12 16
  simulation.addPredator (9, 2) 20 14
  simulation.simulate()
main()
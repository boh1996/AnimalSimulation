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

  while not (validate_file input) do
    writeln "Skriv stien til din settings fil"
    input <- readln()
    clear()
  let settings = new Settings(input)
  let simulation = new Simulation(settings)
  simulation.simulate()
main()
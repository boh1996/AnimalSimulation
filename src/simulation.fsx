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
  let rnd = System.Random()
  for i=1 to settings.numberOfPreys do
    let mutable (x,y) = (rnd.Next(0,settings.width),rnd.Next(0,settings.height))
    while simulation.grid.[x,y] <> None do
      x <- rnd.Next(0,settings.width)
      y <- rnd.Next(0,settings.height)
    simulation.addPrey (x,y)
  for i=1 to settings.numberOfPredators do
    let mutable (x,y) = (rnd.Next(0,settings.width),rnd.Next(0,settings.height))
    while simulation.grid.[x,y] <> None do
      x <- rnd.Next(0,settings.width)
      y <- rnd.Next(0,settings.height)
    simulation.addPredator (x,y)
  simulation.simulate()
main()

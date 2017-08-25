module SVMMain
open System
open System.Globalization
open System.IO
open SVMAST
open ParserUtils
open Microsoft.FSharp.Text.Lexing
open Structure
open ErrorChecker

let parseFile (fileName : string) =
  let inputChannel = new StreamReader(fileName)
  let lexbuf = LexBuffer<char>.FromTextReader inputChannel
  let parsedAST = Parser.start Lexer.tokenstream lexbuf
  parsedAST

let Run SVMState ast =
    let labels = SetLabels ast
    labels
    |>List.iter(fun x -> printfn "%A" x)
    labels        
        

[<EntryPoint>]
let main argv =
  System.Threading.Thread.CurrentThread.CurrentCulture <- CultureInfo.InvariantCulture;
  try
    if argv.Length = 2 then
      do Run CreateNewSVM argv.[1] parseFile argv.[0]   
      let x = System.Console.ReadLine();   
      0
    else
      do printfn "You must specify a command line argument containing the path to the program source file and the size of the memory"
      let x = System.Console.ReadLine();   
      1
  with
  | ParseError(msg,row,col) ->
      do printfn "Parse Error: %s at line %d column %d" msg row col
      let x = System.Console.ReadLine();   
      1
  | :? Exception as e ->
      do printfn "%s" e.Message
      let x = System.Console.ReadLine();   
      1
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

let Check _type =
    match _type with
    | Integer(x, y) -> printfn "integer"
    | Float(x,y) -> printfn "float"
    | String(x,y) -> printfn "string"
    | Address(x)  -> printfn "address"
    | Register(x,y) -> printfn "register"

[<EntryPoint>]
let main argv =
  System.Threading.Thread.CurrentThread.CurrentCulture <- CultureInfo.InvariantCulture;
  try
    if argv.Length = 2 then
      let ast = parseFile argv.[0]
      let state = CreateNewSVM (int argv.[1])      
      let z = {state with MemoryPool = UpdateMemoryValue state 5 (Some(Integer(1,(1,1))))}
      let b = IncreasePC z
      printfn "%A" b
      printfn "%A" ast

      PrintCurrentState b
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
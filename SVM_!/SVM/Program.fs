module SVMMain

open System
open System.Globalization
open System.IO
open SVMAST
open ParserUtils
open SVM
open Microsoft.FSharp.Text.Lexing
open SVMStructure

let parseFile (fileName : string) =
  let inputChannel = new StreamReader(fileName)
  let lexbuf = LexBuffer<char>.FromTextReader inputChannel
  let parsedAST = Parser.start Lexer.tokenstream lexbuf
  parsedAST

let Check _type = 
    match _type with
    | Integer(x,y) -> printfn "integer"
    | Float(x,y) -> printfn "float"
    | String(x,y) -> printfn "string"
    | Address(x)  -> printfn "address"
    | Register(x,y) -> printfn "register"
    | _ -> printfn "oops"

let CheckPosition element =
    match element with
    |Position -> printfn "position"
    | _ -> printfn "oops"


let CheckInstruction instructions =
    instructions
    |> Seq.iter(fun x -> match x with
                                    | Nop x -> printfn "nop"
                                               CheckPosition x
                                    | Mov(x,y,z) ->  printfn "Mov"
                                                     Check x
                                                     Check y  
                                                     CheckPosition z
                                    | And(x,y,z) ->  printfn "And"
                                    | Or(x,y,z)  ->  printfn "Or"
                                    | Not(x,y) ->  printfn "Not"
                                    | Mod(x,y,z) ->  printfn "Mod"
                                    | Add(x,y,z) ->  printfn "Add"
                                    | Sub(x,y,z) ->  printfn "Sub"
                                    | Mul(x,y,z) ->  printfn "Mul"
                                    | Div(x,y,z) ->  printfn "Div"
                                    | Cmp(x,y,z) ->  printfn "CMP"
                                    | Jmp(x,y) ->  printfn "Jmp"
                                    | Jc(x,y,z)  ->  printfn "Jc"
                                    | Jeq(x,y,z) ->  printfn "Jeq"
                                    | Label(x,y) -> printfn "lab"
               )



[<EntryPoint>]
let main argv =
  System.Threading.Thread.CurrentThread.CurrentCulture <- CultureInfo.InvariantCulture;
  try
    if argv.Length = 2 then
      let ast = parseFile argv.[0]
      CheckInstruction ast
      EmptyMemory
      |> Seq.iter(fun x -> printfn "%A" x)                        
      ast
      |> Seq.iter( fun x -> printfn "%A" x)                
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

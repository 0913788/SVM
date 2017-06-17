module ErrorChecker
open SVMAST

let CheckInstruction instructions =
    instructions
    |> Seq.iter(fun x -> match x with
                         | Nop x -> printfn "nop"
                         | Mov(x,y,z) ->  printfn "Mov"
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

let LiteralCheck _type =
    match _type with
    | Integer(x, y) -> printfn "integer"
    | Float(x,y) -> printfn "float"
    | String(x,y) -> printfn "string"
    | Address(x)  -> printfn "address"
    | Register(x,y) -> printfn "register"




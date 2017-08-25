module ErrorChecker
open SVMAST

let LiteralCheck _type =
    match _type with
    | Integer(x, y) -> printfn "integer"
    | Float(x,y) -> printfn "float"
    | String(x,y) -> printfn "string"
    | Address(x)  -> printfn "address"
    | Register(x,y) -> printfn "register"

let rec LabelCheck labels =
    match labels with
    | [] -> ()
    | (_,name)::tail -> 
        if tail 
            |> List.forall (fun (_,name2) -> name <> name2) then LabelCheck tail
        else
            failwithf "It seems like you have used the %s label more then once." name




﻿
module Structure
open SVMAST
type SVMState =
    {
        ProgramCounter : int
        Register1 : option<Literal>
        Register2 : option<Literal>
        Register3 : option<Literal>
        Register4 : option<Literal>
        MemoryPool : option<Literal> list
    }
let CreateNewSVM amountOfMemory =
    {
        ProgramCounter = 0
        Register1 = None
        Register2 = None
        Register3 = None
        Register4 = None
        MemoryPool = List.map(fun _ -> None) [0..amountOfMemory-1]
    }
let UpdateRegister state register value=
    match register with
    | Reg1 -> {state with Register1 = value}
    | Reg2 -> {state with Register2 = value}
    | Reg3 -> {state with Register3 = value}
    | Reg4 -> {state with Register4 = value}

let GetMemoryCell state index =
    state.MemoryPool.[index]

let UpdateMemoryValue state index value =
    let rec Update ind list =
        match ind, list with
        | 0, _::t -> value::t
        | _, h::t -> h::(Update (ind-1) t)
        | _ -> failwith "error"
    Update index state.MemoryPool

let IncreasePC state =
        {state with ProgramCounter = + 1}

let PrintCurrentState state = 
    let CLS = System.Console.Clear()
    let printMemory =
        state.MemoryPool
        |> List.mapi(fun index memCell -> index, memCell)
        |> List.groupBy (fun (index, _) -> index/10)
        |> List.map (fun (_, index) -> index)
    //do CLS
    printfn "%A" printMemory
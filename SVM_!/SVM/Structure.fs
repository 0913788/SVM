
module Structure
open SVMAST
open ErrorChecker

type SVMState =
    {
        ProgramCounter : int
        Register1 : option<Literal>
        Register2 : option<Literal>
        Register3 : option<Literal>
        Register4 : option<Literal>
        MemoryPool : option<Literal> list
    } 

//Initial SVMState
let CreateNewSVM amountOfMemory =
    {
        ProgramCounter = 0
        Register1 = None
        Register2 = None
        Register3 = None
        Register4 = None
        MemoryPool = List.map(fun _ -> None) [0..amountOfMemory-1]
    }

let SetState state _function =
                 {oldState with ProgramCounter = newState.ProgramCounter;
                                MemoryPool = newState.MemoryPool;
                                Register1  = newState.Register1;
                                Register2 = newState.Register2;
                                Register3 = newState.Register3;
                                Register4 = newState.Register4}

let UpdateRegister state register value=
    //Return a new state in which the concerning register is updated with a new value.
    match register with
    | Reg1 -> {state with Register1 = value}
    | Reg2 -> {state with Register2 = value}
    | Reg3 -> {state with Register3 = value}
    | Reg4 -> {state with Register4 = value}

let GetMemoryAddress state index =
    state.MemoryPool.[index]

let UpdateMemoryValue state index value =
    let rec Update _index list =
        match _index, list with
        //When _index is 0; return the new value with the tail of the of the current element.
        | 0, _::t -> value::t
        //When the _index is not 0 return the head with a rec Update call as tail.
        | _, h::t -> h::(Update (ind-1) t)
        //Error handling.
        | _ -> failwith "error"
    Update index state.MemoryPool

let PrintCurrentState state = 
    let CLS = System.Console.Clear()
    let printableMemory =
        state.MemoryPool
        //Retrieves a list which contains indexes and memorycells
        |> List.mapi(fun index memCell -> index, memCell)
        //Creates sublists with a size of 10.
        |> List.groupBy (fun (index, _) -> index/10)
        //Retrieves a list which contains the memorycell info in sublists of 10.
        |> List.map (fun (_, memCellInfo) -> memCellInfo)
    
    let printableRegs = 
               let regs = [state.Register1; state.Register2; state.Register3; state.Register4]
               regs
               //Retrieves the value of registers with the corresponding index.
               |> List.mapi(fun index register -> (index+1, if register.IsSome then register.Value
                                                            else None))
    do CLS
    printfn "Memory"
    printfn "|===========================|"
    printfn "%A" printableMemory
    printfn "Register"
    printfn "|===========================|"
    printfn "%A" printableRegs

let setLabels program =
    let labels =
        program
        //Return a list which includes the index and the instruction.
        |> List.mapi(fun index instruction -> index, instruction)
        //Retrieve the lable calls.
        |> List.filter(fun (index, instruction) -> match instruction with
                                                         |Label(x,y) -> true
                                                         |_ -> false)
        //Retrieve the lable names.
        |>List.map(fun(index, instruction) -> index, match instruction with 
                                                           |Label(name,_) -> name
                                                           |_ -> failwith "error")
    labels
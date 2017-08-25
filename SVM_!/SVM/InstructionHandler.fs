module InstructionHandler

let IncreasePC state =
        {state with ProgramCounter = + 1}

let ExecuteProgram program labels state =
    let NextInstruction = 
        if state.ProgramCounter =>0 then Nop(0,0)
        else
            program
            |> List.item state.ProgramCounter
    match NextInstruction with
    |Nop(0,0) -> None
    |_ -> Some(state,
        match NextInstruction with
        | Nop(position)                      -> NOP state position
        | Mov(literal1, literal2 , position) -> MOVE state literal1 literal2 position
        | And(register, literal1 , position) -> AND state register literal1 position
        | Or(register, literal1 , position)  -> OR state register literal1 position
        | Not(register, position)            -> NOT state register position
        | Mod(register, literal1 , position) -> MOD state register literal1 position
        | Add(register, literal1 , position) -> ADD state register literal1 position
        | Sub(register, literal1 , position) -> SUB state register literal1 position
        | Mul(register, literal1 , position) -> MUL state register literal1 position
        | Div(register, literal1 , position) -> DIV state register literal1 position
        | Cmp(register, literal1 , position) -> CMP state register literal1 position
        | Jmp(label, position)               -> JMP state label position
        | Jc(label, register, position)      -> JC state label register position
        | Jeq(label, register, position)     -> JEQ state label register position
        | Label(label, position)             -> IncreasePC state
        )

let NOP state position =
let MOVE state literal1 literal2 position =
let AND  state register literal1 position =
let OR state register literal1 position =
let NOT state register position =
let MOD state register literal1 position =
let ADD state register literal1 position =
let SUB state register literal1 position =
let MUL state register literal1 position =
let DIV state register literal1 position =
let CMP state register literal1 position =
let JMP state label position =
let JC state label register position =
let JEQ state label register position =
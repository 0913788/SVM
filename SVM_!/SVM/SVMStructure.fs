module SVMStructure
open SVMAST

type SVMElements =
    |MemoryCell of option<Literal> * int
    |ProgramCounter of int


let EmptyMemory =
    let memory = Array.create 20 (MemoryCell(None,0))
    for i in 0..memory.Length-1 do 
        Array.set memory i (MemoryCell(None,i))
    memory


    
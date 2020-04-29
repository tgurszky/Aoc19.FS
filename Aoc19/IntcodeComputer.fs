module IntcodeComputer

type OpCode = Add = 1 | Multiply = 2 | Halt = 99
type Program = int array
type ExecutionContext = {
    currentOp: OpCode option
    operandA: int option
    operandB: int option
    state: Program
}

let identifyOperation item =
    match item with
    | 1 -> Some OpCode.Add
    | 2 -> Some OpCode.Multiply
    | 99 -> Some OpCode.Halt
    | _ -> None

let update index value state =
    Array.mapi (fun i v -> if i = index then value else v) state

let handleTwoOperands operation context current =
    match context.operandA with
    | None -> { context with operandA = Some (Array.item current context.state) }
    | Some opAVal -> match context.operandB with
                        | None -> { context with operandB = Some (Array.item current context.state) }
                        | Some opBVal -> { 
                            currentOp = None; 
                            operandA = None; 
                            operandB = None; 
                            state = update current (operation opAVal opBVal) context.state }


let executeStep context index =
    match context.currentOp with
        | None -> { context with currentOp = identifyOperation (Array.item index context.state) }
        | Some OpCode.Add -> handleTwoOperands (+) context (Array.item index context.state)
        | Some OpCode.Multiply -> handleTwoOperands (*) context (Array.item index context.state)
        | Some _ -> context

let run program =
    let initialContext = { currentOp = None; operandA = None; operandB = None; state = program }
    let { state = result } = [ 0 .. program.Length - 1] |> List.fold executeStep initialContext
    result

module IntcodeComputer

type OpCode = Add | Multiply | Halt
type Program = int array
type ExecutionContext = {
    CurrentOp: OpCode option
    OperandA: int option
    OperandB: int option
    State: Program
}

let identifyOperation item =
    match item with
    | 1 -> Some Add
    | 2 -> Some Multiply
    | 99 -> Some Halt
    | _ -> None

let update index value state =
    Array.mapi (fun i v -> if i = index then value else v) state

let handleTwoOperands operation context current =
    match context.OperandA with
    | None -> { context with OperandA = Some (Array.item current context.State) }
    | Some opAVal -> match context.OperandB with
                        | None -> { context with OperandB = Some (Array.item current context.State) }
                        | Some opBVal -> { 
                            CurrentOp = None; 
                            OperandA = None; 
                            OperandB = None; 
                            State = update current (operation opAVal opBVal) context.State }


let executeStep context index =
    match context.CurrentOp with
        | None -> { context with CurrentOp = identifyOperation (Array.item index context.State) }
        | Some Add -> handleTwoOperands (+) context (Array.item index context.State)
        | Some Multiply -> handleTwoOperands (*) context (Array.item index context.State)
        | Some _ -> context

let run program =
    let initialContext = { CurrentOp = None; OperandA = None; OperandB = None; State = program }
    let { State = result } = [ 0 .. program.Length - 1] |> List.fold executeStep initialContext
    result

module Study.Elmish.Engine

open ParserCombinators

let init() =
  {
    Rows = [1 .. 15]
    Cols = ['A' .. 'K']
    Active = None
    Cells = Map.empty
  }
  
let update evt state =
  match evt with
  | StartEdit pos ->  
      { state with Active = Some pos }
  | UpdateValue (pos, value) ->
      let newCells = Map.add pos value state.Cells
      { state with Cells = newCells }

let operator =
  pChar '+' <|> pChar '\\' <|> pChar '-' <|> pChar '*' <|> pChar '/' 

let number = pInteger |>> Number

let reference = anyOf ['A'..'Z'] .>>. pDigit |>> fun (col,row) -> Reference (col, row)

let binary = (number <|> reference) .>>. operator .>>. (number <|> reference) |>> (fun ((l,op), r) -> Binary (l,op,r))

let expr = binary <|> number <|> reference

let cellValue = (pChar '=' >>. expr) <|> number

let rec evaluate state = function
  | Number n -> Ok n
  | Binary (l,op,r) ->
      match (op, (evaluate state l), (evaluate state r)) with
      | ('+', Ok lv, Ok rv) -> Ok ((+) lv rv)
      | ('-', Ok lv, Ok rv) -> Ok ((-) lv rv)
      | ('*', Ok lv, Ok rv) -> Ok ((*) lv rv)
      | ('/', Ok lv, Ok rv) -> Ok ((/) lv rv)
      | (_, Ok _, Ok _) -> Error (sprintf "%c: operator not supported" op) 
      | (_, Error e, _) -> Error e 
      | (_, _, Error e) -> Error e 
  | Reference r ->
      match (state.Cells.TryFind r) with
      | None -> Error "#NA"
      | Some v ->
          match run cellValue v with
          | Ok (exp,_) -> evaluate state exp
          | Error e -> Error e
          
let pEvaluate state value =
  match (run cellValue value) with
  | Ok (exp,_) -> Result.bind (fun x -> Ok (string x)) (evaluate state exp)
  | Error e -> Error e
module Study.Elmish.ParserCombinators

open System

type ParsingPosition = {
  line: int
  column: int
  index: int
}

type InputState = {
  content: string
  position: ParsingPosition
}

type ParserPosition = {
  currentLine: string
  line: int
  column: int  
}

type ParserLabel = string

type ParserError = string

type ParserResult<'a> = Result<('a*InputState), (ParserLabel * ParserError * ParserPosition)>

type Parser<'a> = {
  parse: InputState -> ParserResult<'a>
  label: ParserLabel
}

let stateToParserPosition inputState =
  { currentLine = inputState.content; line = inputState.position.line; column = inputState.position.column }

let initialPos = { line = 0; column = 0; index = 0; }

let fromStr str =
    if isNull str then
      { content = ""; position = initialPos }
    else
      { content = str; position = initialPos }

let rec nextChar input =
  if (input.position.index >= input.content.Length) then
    input, None
  else
    match input.content.[input.position.index] with
    | '\r' -> nextChar { input with position = { line = input.position.line; column = input.position.column; index = input.position.index + 1 } }
    | '\n' -> { input with position = { line = input.position.line + 1; column = 0; index = input.position.index + 1 } }, Some '\n'
    | c -> { input with position = { line = input.position.line; column = input.position.column + 1; index = input.position.index + 1 } }, Some c
  
let setLabel parser newLabel =
  let { parse = parse } = parser
  { parse = parse ; label = newLabel }

let (<?>) = setLabel

let run parser =
  let {parse = pFn} = parser
  pFn

let parseWith parser =
  fromStr >> run parser
  
let result (r: ParserResult<'a>) =
  match r with
  | Ok (e,r) -> sprintf "%A" e
  | Error (label, error, position) -> sprintf "Error parsing %s at Line: %d Col: %d \n%s\n%*s^ %s" label position.line position.column position.currentLine position.column "" error

let printResult (r: ParserResult<'a>) = result r |> printf "%s"

let bind f p =
  let innerFn input =
    match (run p input) with
    | Error e -> Error e
    | Ok (e,r) -> run (f e) r
  { parse = innerFn ; label = "" }

let (>>=) p f = bind f p

let pReturn x =
  let innerFn input =
    Ok (x, input)
  { parse = innerFn ; label = "" }
  
let map f =
  bind (f >> pReturn)

let (<!>) = map

let (|>>) x f = map f x

let andThen parser1 parser2 =
  let defaultLabel = sprintf "%s then %s" parser1.label parser2.label
  parser1 >>= (fun e1 -> parser2 >>= (fun e2 -> pReturn (e1,e2))) <?> defaultLabel

let (.>>.) = andThen

let orElse parser1 parser2 =
  let parser12 input =
    match (run parser1 input) with
    | Ok (expected1, remaining) ->
        Ok (expected1, remaining)
    | Error e1 ->
        match (run parser2 input) with
        | Error e2 -> Error e2 
        | Ok (expected2, remaining) ->
            Ok (expected2, remaining)
  { parse = parser12 ; label = sprintf "%s or %s" parser1.label parser2.label }
  
let (<|>) = orElse

let choice parserList =
  List.reduce (<|>) parserList

let (.>>) p1 p2 =
  map (fun (e1,e2) -> e1) (p1 .>>. p2)

let (>>.) p1 p2 =
  map (fun (e1,e2) -> e2) (p1 .>>. p2)
  
let apply pFn parser =
  pFn >>= (fun f -> parser >>= (fun x -> pReturn (f x)))
  
let (<*>) = apply

let rec pSequence (parsers: Parser<'a> list) =
  match parsers with
    | p :: tail -> p .>>. (pSequence tail) |>> (fun (a,b) -> a::b)
    | [] -> pReturn []
  
let rec any parser input =
  match (run parser input) with
  | Error _ -> ([], input)
  | Ok (e1, r1) ->
      let (e2,r2) = any parser r1
      (e1 :: e2, r2)

let many parser =
  let fnMany input =
    Ok (any parser input)
  { parse = fnMany ; label = sprintf "many %s" parser.label }

let atLeastOne parser =
  let fnMany input =
    match (run parser input) with
    | Error e -> Error e
    | Ok (e1, r1) ->
        let (e2, r2) = any parser r1
        Ok (e1 :: e2, r2)
  { parse = fnMany ; label = sprintf "at least one %s" parser.label }

let opt parser =
  (parser |> map Some) <|> pReturn None

//
//
//
//
//

let satisfy predicate label =
  let pFn input =
      match nextChar input with
      | remaining, None -> Error (label, "reached end of stream",  stateToParserPosition input)
      | remaining, Some current -> 
        if (predicate current) then
          Ok (current, remaining)
        else
          Error (label, sprintf "unexpected '%c'" current, stateToParserPosition input)
  { parse = pFn ; label = label }

let pChar expectedChar = satisfy (fun c -> c = expectedChar) (sprintf "char '%c'" expectedChar)

// match a specific string
let paString stringToMatch =
  let listToString chars =
    String (List.toArray chars)
  stringToMatch |> Seq.map pChar |> Seq.toList |> pSequence |> map listToString 
  
let anyOf chars =
  chars |> List.map pChar |> List.reduce orElse

let pDigit =
  let toInt = function
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | _ ->  failwith "check your parser, this case should not happen"
  anyOf ['0'..'9'] |> map toInt <?> "digit"
  
let pInteger =
  let listToInt (sign, ints) =
    let absoluteValue = ints |> List.mapi (fun rank x ->  x * (pown 10 (ints.Length - 1 - rank))) |> List.sum
    match sign with
    | Some _ -> - absoluteValue
    | None -> absoluteValue
  opt (pChar '-') .>>. atLeastOne pDigit |> map listToInt <?> "integer"

let between wrapper parser = wrapper >>. parser .>> wrapper <?> sprintf "between %s" wrapper.label

let between2 wrapper1 wrapper2 parser = wrapper1 >>. parser .>> wrapper2 <?> sprintf "between %s and %s" wrapper1.label wrapper2.label

let manySpaces = many (anyOf [' '; '\t'] <?> "whitespaces")

let quoted (_:Parser<'a>) = between (pChar '\'' <?> "quotes")

let doubleQuoted (_:Parser<'a>) = between (pChar '"' <?> "doubleQuotes")

let separatedBy parser separator =
  many (parser .>> separator)

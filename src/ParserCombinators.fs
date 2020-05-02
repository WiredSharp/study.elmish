module Study.Elmish.ParserCombinators

open System

type Parser<'a> = Parser of (string -> Result<('a*string), string>)
  
let run parser input =
  let (Parser pFn) = parser
  pFn input

let andThen parser1 parser2 =
  let nextParsing (expected1, remaining) =
    Result.bind (fun (e,r) -> Ok((expected1, e), r)) (run parser2 remaining)
  let parser12 input =
    Result.bind nextParsing (run parser1 input)
  Parser parser12

let (.>>.) = andThen

let orElse parser1 parser2 =
  let parser12 input =
    match (run parser1 input) with
    | Ok (expected1, remaining) ->
        Ok (expected1, remaining)
    | Error e ->
        match (run parser2 input) with
        | Error e -> Error e
        | Ok (expected2, remaining) ->
            Ok (expected2, remaining)
  Parser parser12
  
let (<|>) = orElse

let choice parserList =
  List.reduce (<|>) parserList
      
let map f parser =
  let mappedParser input =
    match (run parser input) with
    | Error e -> Error e
    | Ok (e,r) ->  Ok(f e, r)
  Parser mappedParser

let (<!>) = map

let (|>>) x f = map f x

let pReturn x =
  let innerFn input =
    Ok (x, input)
  Parser innerFn
  
let apply pFn parser =
  (pFn .>>. parser) |> map (fun (f,x) -> f x)
  
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
  Parser fnMany

let atLeastOne parser =
  let fnMany input =
    match (run parser input) with
    | Error e -> Error e
    | Ok (e1, r1) ->
        let (e2, r2) = any parser r1
        Ok (e1 :: e2, r2)
  Parser fnMany

let opt parser =
  (parser |> map Some) <|> pReturn None

//
//
//
//
//

let pChar expectedChar =
  let pFn input =
    if String.IsNullOrEmpty(input) then
      Error (sprintf "expected %c but reach end of input" expectedChar)
    else
      if input.[0] = expectedChar then
        Ok (expectedChar, input.[1..])
      else
        Error (sprintf "expected %c but got %c" expectedChar input.[0])
  Parser pFn


// match a specific string
let pString stringToMatch =
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
  anyOf ['0'..'9'] |> map toInt
  
let pInteger =
  let listToInt (sign, ints) =
    let absoluteValue = ints |> List.mapi (fun rank x ->  x * (pown 10 (ints.Length - 1 - rank))) |> List.sum
    match sign with
    | Some _ -> - absoluteValue
    | None -> absoluteValue
  opt (pChar '-') .>>. atLeastOne pDigit |> map listToInt

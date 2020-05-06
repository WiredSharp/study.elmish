module Study.Elmish.Views

open ParserCombinators
open Engine
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
open Fulma
open Study.Elmish

let renderCellEdit position dispatch value =
  td [ Class "selected"] [
    input [
      OnChange (fun ev -> dispatch (UpdateValue (position, ev.target?value) ))
      AutoFocus true
      Value value
      Placeholder "enter value"
      Type "text" ]
  ]

let renderCellView position dispatch value =
  td [
    OnClick (fun _ -> dispatch (StartEdit position))
  ] [str value]


let renderCell spreadSheet position dispatch =
  let cell = spreadSheet.Cells.TryFind position
  if spreadSheet.Active = Some position then
    renderCellEdit position dispatch (defaultArg cell "")
  else
    let content =
      match cell with
      | None -> Ok ""
      | Some value -> pEvaluate spreadSheet value
    match content with
    | Error e -> renderCellView position dispatch (sprintf "%s: %s" "#ERR" e)
    | Ok c -> renderCellView position dispatch c
  
let renderView state dispatch =
  Container.container [] [
    table [] [
      tbody [] [
        yield tr [] [
          yield th [] []
          for col in state.Cols -> th [] [ str (string col) ]
        ]
        for row in state.Rows -> tr [] [
          yield th [] [str (string row)]
          for col in state.Cols -> renderCell state (col,row) dispatch
        ]
      //yield! state.Rows |> List.mapi (fun idx cell -> tr [] [str (string idx)])
      ]
    ]
  ]
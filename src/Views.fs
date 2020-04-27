module Study.Elmish.Views

open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
open Fulma

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
  let render = if spreadSheet.Active = Some position then
                  renderCellEdit position dispatch
                 else
                  renderCellView position dispatch
  let value = spreadSheet.Cells.TryFind position
  render (defaultArg value "")
  
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
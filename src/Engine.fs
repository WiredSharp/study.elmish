module Study.Elmish.Engine

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

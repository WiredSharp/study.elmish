namespace Study.Elmish

type Position = char * int

type Spreadsheet =
    { Cols : char list
      Rows : int list
      Active : Position option
      Cells : Map<Position, string> }
    
type Event =
  | StartEdit of Position
  | UpdateValue of Position * string
  
type Expr =
  | Number of int
  | Binary of Expr * char * Expr

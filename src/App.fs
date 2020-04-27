module Study.Elmish.App

open Elmish
open Elmish.React
open Elmish.Debug
open Engine
open Views

// App
Program.mkSimple init update renderView
#if DEBUG
  |> Program.withConsoleTrace
#endif
//#if DEBUG
//  |> Program.withReactBatched "elmish-app"
//  |> Program.withDebugger
//#else
//  |> Program.withReactBatched "elmish-app"
//#endif
  |> Program.withReactBatched "elmish-app"
  |> Program.run
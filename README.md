# Tomas Petricek Fable spreadsheet study

see [Build your own Excel 365 in an hour with F#](https://www.youtube.com/watch?v=Bnm71YEt_lI)

This project use the [ELM architecture](https://guide.elm-lang.org/architecture/).

As the code run on client side, i use [Fable]() to generate Javascript from F# code.

i used the [sample memory project](https://elmish.github.io/#samples/memory) as a starter.

## Build and running the app

1. Install npm dependencies: `yarn install` or `npm install`
1. Install dotnet dependencies: `dotnet paket restore`
1. you can run two separate commands:
    1. In one shell, start Fable daemon: `cd src && dotnet fable start`
    1. In another shell, Start Webpack dev server: `npm start`
1. or as a single command: `dotnet fable npm-run start`
1. In your browser, open: http://localhost:8080/

## Further readings

- a DSL to generate HTML from F#: [FsHtml](https://github.com/y2k/FsHtml/blob/master/Examples.fsx)
- Fable tutorial on [medium.com](https://medium.com/@zaid.naom/f-interop-with-javascript-in-fable-the-complete-guide-ccc5b896a59f)
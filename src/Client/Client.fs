module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Shared

open Fulma
open Fulma.BulmaClasses


type Model =
    { Counter : Counter option
      GraphModel : Graph.Model }

type Msg =
| Increment
| Decrement
| Init of Result<Counter, exn>
| GraphMessage of Graph.Msg


let init () : Model * Cmd<Msg> =
    let gm, gc = Graph.init ()
    let model =
        { GraphModel = gm
          Counter = None }
    let cmd =
        Cmd.ofPromise
            (fetchAs<int> "/api/init")
            []
            (Ok >> Init)
            (Error >> Init)
    model, Cmd.batch [ cmd; gc |> Cmd.map GraphMessage ]

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    let model', cmd =
        match model.Counter,  msg with
        | Some x, Increment -> { model with Counter = Some (x + 1) }, Cmd.none
        | Some x, Decrement -> { model with Counter = Some (x - 1) }, Cmd.none
        | None, Init (Ok x) -> { model with Counter = Some x }, Cmd.none
        | _, GraphMessage gmessage ->
            let gm, gc = Graph.update gmessage model.GraphModel
            {model with GraphModel = gm }, gc |> Cmd.map GraphMessage
        | _ -> { model with Counter = None }, Cmd.none
    model', cmd

let safeComponents =
  let intersperse sep ls =
    List.foldBack (fun x -> function
      | [] -> [x]
      | xs -> x::sep::xs) ls []

  let components =
    [
      "Giraffe", "https://github.com/giraffe-fsharp/Giraffe"
      "Fable", "http://fable.io"
      "Elmish", "https://fable-elmish.github.io/"
      "Fulma", "https://mangelmaxime.github.io/Fulma"
    ]
    |> List.map (fun (desc,link) -> a [ Href link ] [ str desc ] )
    |> intersperse (str ", ")
    |> span [ ]

  p [ ]
    [ strong [] [ str "SAFE Template" ]
      str " powered by: "
      components ]

let show = function
| Some x -> string x
| None -> "Loading..."

let button txt onClick =
  Button.button
    [ Button.IsFullWidth
      Button.Color IsPrimary
      Button.OnClick onClick ]
    [ str txt ]

let view (model : Model) (dispatch : Msg -> unit) =
  let handleGraphDispatch (msg : Graph.Msg) : Msg =
    GraphMessage msg

  div []
    [ Navbar.navbar [ Navbar.Color IsPrimary ]
        [ Navbar.Item.div [ ]
            [ Heading.h2 [ ]
                [ str "SAFE Template" ] ] ]

      Container.container []
        [ Content.content [ Content.CustomClass Bulma.Properties.Alignment.HasTextCentered ]
            [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model.Counter) ] ]
          Columns.columns []
            [ Column.column [] [ button "-" (fun _ -> dispatch Decrement) ]
              Column.column [] [ button "+" (fun _ -> dispatch Increment) ] ] ]

      Container.container []
        [ Graph.view model.GraphModel (handleGraphDispatch >> dispatch) ]

      Footer.footer [ ]
        [ Content.content [ Content.CustomClass Bulma.Properties.Alignment.HasTextCentered ]
            [ safeComponents ] ] ]


// #if DEBUG
// open Elmish.Debug
// open Elmish.HMR
// #endif

// Program.mkProgram init update view
// #if DEBUG
// |> Program.withConsoleTrace
// |> Program.withHMR
// #endif
// |> Program.withReact "elmish-app"
// #if DEBUG
// |> Program.withDebugger
// #endif
// |> Program.run

module Client

open Elmish

open Fable.Helpers.React

open Fulma
open Fulma.BulmaClasses


type Model =
    { GraphModel : Graph.Model }

type Msg =
| GraphMessage of Graph.Msg

let init () : Model * Cmd<Msg> =
    let gm, gc = Graph.init ()
    let model =
        { GraphModel = gm }
    model, Cmd.batch [ gc |> Cmd.map GraphMessage ]

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    let model', cmd =
        match msg with
        | GraphMessage gmessage ->
            let gm, gc = Graph.update gmessage model.GraphModel
            {model with GraphModel = gm }, gc |> Cmd.map GraphMessage
    model', cmd

let view (model : Model) (dispatch : Msg -> unit) =
  let handleGraphDispatch (msg : Graph.Msg) : Msg =
    GraphMessage msg

  div []
    [ Navbar.navbar [ Navbar.Color IsPrimary ]
        [ Navbar.Item.div [ ]
            [ Heading.h2 [ ]
                [ str "SAFE Template" ] ] ]

      Container.container []
        [ Graph.view model.GraphModel (handleGraphDispatch >> dispatch) ]

      Footer.footer [ ]
        [ Content.content [ Content.CustomClass Bulma.Properties.Alignment.HasTextCentered ]
            [ str "footer" ] ] ]

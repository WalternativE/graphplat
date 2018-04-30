module Login

open Fulma
open Global
open Elmish
open Fable.Core.JsInterop
open Shared
open System
open Fable.PowerPack
open Fable.PowerPack.Fetch.Fetch_types

module R = Fable.Helpers.React
module RP = Fable.Helpers.React.Props

type Model =
    { User : UserData option
      Email : string
      Password : string
      ErrorMsg : string }

type Message =
    | SubmitLogin
    | SetEmail of string
    | SetPassword of string
    | AuthError of exn
    | LoginSuccess of TokenResult

type ExternalMessage =
    | NoOp
    | UserLoggedIn of UserData

let init (user : UserData option) =
    { User = user; Email = null; Password = null; ErrorMsg = null }, Cmd.none

let authUser (creds : LoginCredentials) =
    promise {
        if String.IsNullOrWhiteSpace creds.Email then failwith "Please supply email"
        if String.IsNullOrWhiteSpace creds.Password then failwith "Please supply password"

        let body = toJson creds

        let props =
            [ RequestProperties.Method HttpMethod.POST
              Fetch.requestHeaders
                [ HttpRequestHeaders.ContentType "application/json" ]
              RequestProperties.Body !^body ]

        try
            return! Fetch.fetchAs<TokenResult> "/api/token" props
        with _ ->
            return failwithf "Cold not authenticate user."
    }

let authUserCmd creds =
    Cmd.ofPromise authUser creds LoginSuccess AuthError


let update (msg : Message) (model : Model) =
    match msg with
    | SubmitLogin ->
        printfn "SubmitLogin received %A" model
        let creds =
            { Email = model.Email
              Password = model.Password }
        model, authUserCmd creds, NoOp
    | SetEmail e -> { model with Email = e }, Cmd.none, NoOp
    | SetPassword p -> { model with Password = p }, Cmd.none, NoOp
    | AuthError e ->
        { model with ErrorMsg = e.Message }, Cmd.none, NoOp
    | LoginSuccess token ->
        let ud = { UserName = model.Email; Token = token.Token }
        { User = Some ud; Email = ""; Password = ""; ErrorMsg = "" }, Cmd.none, UserLoggedIn ud

let displayProps model =
    let hideNotification = String.IsNullOrWhiteSpace model.ErrorMsg
    if hideNotification then  [ RP.Display "none" ] else []

let viewError model =
    Notification.notification [ Notification.Color IsDanger; Notification.Props [  RP.Style (displayProps model) ] ]
        [ R.str model.ErrorMsg ]

let view model dispatch =
    Section.section []
        [ Container.container []
            [ viewError model
              R.form []
                [ Field.div []
                    [ Label.label []
                        [ R.str "Email" ]
                      Control.div []
                        [ Input.email
                            [ Input.Id "login-email"
                              Input.DefaultValue model.Email
                              Input.Placeholder "bob@bobbing.com"
                              Input.OnChange (fun ev -> dispatch (SetEmail !!ev.target?value))
                              Input.Props [ RP.AutoFocus true ] ] ] ]
                  Field.div []
                    [ Label.label []
                        [ R.str "Password" ]
                      Control.div []
                        [ Input.password
                            [ Input.Id "login-password"
                              Input.DefaultValue model.Password
                              Input.OnChange (fun ev -> dispatch (SetPassword !!ev.target?value)) ] ] ]
                  Field.div [ Field.IsGrouped ]
                    [ Control.div []
                        [ Button.button [ Button.Color IsPrimary; Button.OnClick (fun _ -> dispatch SubmitLogin) ]
                            [ R.str "Submit" ] ]
                      Control.div []
                        [ Button.button [ Button.Color IsWhite ]
                            [ R.str "Cancel" ] ] ] ] ] ]

module Login

open Fulma
open Global
open Elmish
open Fable.Core.JsInterop
open Shared
open System

module R = Fable.Helpers.React
module RP = Fable.Helpers.React.Props

type Model =
    { UserData : UserData option
      Email : string
      Password : string
      ErrorMsg : string }

type Message =
    | AcquireToken
    | SetEmail of string
    | SetPassword of string
    | AuthError of exn
    | TokenAcquired of TokenResult
    | UserDataResolved of UserData

type ExternalMessage =
    | NoOp
    | UserLoggedIn of UserData

let init (user : UserData option) =
    { UserData = user; Email = null; Password = null; ErrorMsg = null }, Cmd.none

let authUserCmd creds =
    Cmd.ofPromise ApiClient.authUser creds TokenAcquired AuthError

let acquireUserDataCmd tr =
    Cmd.ofPromise ApiClient.acquireUserData tr UserDataResolved  AuthError

let update (msg : Message) (model : Model) =
    match msg with
    | AcquireToken ->
        let creds =
            { Email = model.Email
              Password = model.Password }
        model, authUserCmd creds, NoOp
    | SetEmail e -> { model with Email = e }, Cmd.none, NoOp
    | SetPassword p -> { model with Password = p }, Cmd.none, NoOp
    | AuthError e ->
        { model with ErrorMsg = e.Message }, Cmd.none, NoOp
    | TokenAcquired token ->
        model, acquireUserDataCmd token, NoOp
    | UserDataResolved ud ->
        { model with UserData = Some ud; Email = ""; Password = "" }, Cmd.none, UserLoggedIn ud


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
                        [ Button.button [ Button.Color IsPrimary; Button.OnClick (fun _ -> dispatch AcquireToken) ]
                            [ R.str "Submit" ] ]
                      Control.div []
                        [ Button.button [ Button.Color IsWhite ]
                            [ R.str "Cancel" ] ] ] ] ] ]

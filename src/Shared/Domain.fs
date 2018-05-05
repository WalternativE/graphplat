namespace Domain

open System

module Model =

    type Email = string
    type UserName = string

    type User =
        { Id : Guid
          Name : UserName
          Email : Email }

    type Workspace =
        { Id : Guid
          Name : string }

    type UserSpace =
        { Id : Guid
          User : User
          Workspaces : Workspace list }

module Events =

    open Model

    type UserError =
        | EmailAlreadyUsed of Email

    type UserEvent =
        | UserCreated of User
        | UserError of UserError

    type Error =
        | AlreadyExists of UserSpace
        | NoUserSpace
        | WsNameNotUnique of Workspace
        | WsNotFound of Workspace

    let errorToString (e : Error) =
        match e with
        | AlreadyExists us -> sprintf "The userspace for user %s already exists." us.User.Name
        | NoUserSpace -> "No userspace exists which can be used for operations."
        | WsNameNotUnique ws -> sprintf """The name "%s" you chose for your new workspace is already used.""" ws.Name
        | WsNotFound ws -> sprintf "The workspace with the id %A could not be found." ws.Id

    type Event =
        | UserSpaceCreated of UserSpace
        | WorkspaceCreated of Workspace
        | WorkspaceDeleted of Workspace
        | Error of Error

module EventStore =

    type StreamedStoreEvent<'a> =
        { Id : Guid
          StreamId : Guid
          Event : 'a }

module Queries =

    type UserQuery =
        | GetUser of Guid

    type SpacesQuery =
        | GetUserspace of Guid

module Commands =

    open Model

    type UserCommand =
        | CreateUser of User

    type SpacesCommand =
        | CreateUserSpace of User
        | CreateWorkSpace of Workspace
        | DeleteWorkSpace of Workspace

module Projection =

    open Model
    open Events

    let state folder (givenHistory : 'b List) (model : 'c) =
        givenHistory
        |> List.fold folder model

    let applyToUser (user : User) (ue : UserEvent) =
        match ue with
        | UserCreated _ ->
            user
        | UserError _ ->
            user

    let userState = state applyToUser

    let applyToUserSpace (userSpace : UserSpace) (e : Event) =
        match e with
        | UserSpaceCreated _ ->
            userSpace
        | WorkspaceCreated ws ->
            { userSpace with Workspaces = ws::userSpace.Workspaces }
        | WorkspaceDeleted ws ->
            let wss =
                userSpace.Workspaces
                |> List.filter (fun w -> w.Id = ws.Id |> not)
            { userSpace with Workspaces = wss }
        | Error _ ->
            userSpace

    let userSpacesState = state applyToUserSpace

    let initialUserSpace (history : Event list) =
       match history with
       | [] -> None
       | x::_ ->
            match x with
            | UserSpaceCreated us -> Some us
            | _ -> None

module Behaviour =

    open Model
    open Events
    open Commands
    open Projection

    let createUserSpace (user : User) =
        { Id = user.Id
          User = user
          Workspaces = [] }

    let handleCommand (events : Event list) (command : SpacesCommand) =
        match command with
        | CreateUserSpace user ->
            match initialUserSpace events with
            | Some us ->
                AlreadyExists us |> Error
            | None ->
                let s = createUserSpace user
                let ev = UserSpaceCreated s
                ev
        | CreateWorkSpace ws ->
            match initialUserSpace events with
            | None -> Error NoUserSpace
            | Some us ->
                let currentUs = userSpacesState events us
                let wsExists =
                    currentUs.Workspaces
                    |> List.exists (fun w -> w.Name = ws.Name)
                if wsExists then
                    WsNameNotUnique ws |> Error
                else
                    WorkspaceCreated ws
        | DeleteWorkSpace ws ->
            match initialUserSpace events with
            | None -> Error NoUserSpace
            | Some us ->
                let currentUs = userSpacesState events us
                let wsCanBeFound =
                    currentUs.Workspaces
                    |> List.exists (fun w -> w.Id = ws.Id)
                if wsCanBeFound then
                    WorkspaceDeleted ws
                else
                    WsNotFound ws |> Error

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

    let errorToString (e : Error) =
        match e with
        | AlreadyExists us -> sprintf "The userspace for user %s already exists." us.User.Name

    type Event =
        | UserSpaceCreated of UserSpace
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

module Behaviour =

    open Model

    let createUserSpace (user : User) =
        { Id = user.Id
          User = user
          Workspaces = [] }


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
        | Error _ ->
            userSpace

    let userSpacesState = state applyToUserSpace

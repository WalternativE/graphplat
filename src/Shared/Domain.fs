module Domain

open System

module Model =

    type Email = string

    type User =
        { Id : Guid
          Email : Email }

    type Workspace =
        { Id : Guid
          Name : string }

    type UserSpace =
        { User : User
          Workspaces : Workspace list }

module Events =

    open Model

    // users are independet from user spaces
    // currently there is no need to stream user events (not enough action to merit the hassle)
    type UserEvent =
        | UserCreated of User
        | UserEmailChanged of User

    type Event =
        | UserSpaceCreated of UserSpace

module EventStore =

    open Events

    type Streamed =
        { Id : Guid
          StreamId : Guid
          Event : Event }

module Queries =

    open Model

    type Query =
        | GetUser of Email
        | GetUserspace of User


module Behaviour =

    open Model
    open Events


module Projection =

    open Model
    open Events

namespace Domain

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

    type IEvent = interface
        end

    // users are independet from user spaces
    // currently there is no need to stream user events (not enough action to merit the hassle)
    type UserEvent =
        | UserCreated of User
        | UserEmailChanged of User

        with interface IEvent

    type Event =
        | UserSpaceCreated of UserSpace

        with interface IEvent

module EventStore =

    open Events

    type StoreEvent<'a when 'a :> IEvent> =
        { Id : Guid
          Event : 'a }

    type StreamedStoreEvent<'a when 'a :> IEvent> =
        { Id : Guid
          StreamId : Guid
          Event : 'a }

module Queries =

    open Model

    type Query =
        | GetUser of Email
        | GetUserspace of User


// module Behaviour =

//     open Model
//     open Events


// module Projection =

//     open Model
//     open Events

#r @"C:\dev\code\graphplat\packages\fsi\NETStandard.Library.NETFramework\build\net461\lib\netstandard.dll"
// #r @"C:\Users\Gregor\.nuget\packages\aether\8.2.0\lib\net45\Aether.dll"
#r @"C:\Users\Gregor\.nuget\packages\baseline\1.5.0\lib\netstandard2.0\Baseline.dll"
#r @"C:\Users\Gregor\.nuget\packages\marten\2.7.1\lib\netstandard2.0\Marten.dll"
#r @"C:\Users\Gregor\.nuget\packages\marten.fsharp\0.1.1\lib\netstandard1.6\Marten.FSharp.dll"
#r @"C:\Users\Gregor\.nuget\packages\system.threading.tasks.extensions\4.4.0\lib\netstandard2.0\System.Threading.Tasks.Extensions.dll"
#r @"C:\Users\Gregor\.nuget\packages\newtonsoft.json\11.0.2\lib\netstandard2.0\Newtonsoft.Json.dll"
#r @"C:\Users\Gregor\.nuget\packages\npgsql\3.2.7\lib\netstandard2.0\Npgsql.dll"
#r @"C:\Users\Gregor\.nuget\packages\remotion.linq\2.2.0\lib\netstandard1.0\Remotion.Linq.dll"
#r @"C:\Users\Gregor\.nuget\packages\system.reflection.emit.lightweight\4.3.0\lib\netstandard1.3\System.Reflection.Emit.Lightweight.dll"
#r @"C:\Users\Gregor\.nuget\packages\system.threading.tasks.dataflow\4.8.0\lib\netstandard2.0\System.Threading.Tasks.Dataflow.dll"
#r @"C:\Users\Gregor\.nuget\packages\marten.fsharp\0.3.0\lib\netstandard2.0\Marten.FSharp.dll"
// #load @"c:\dev\code\graphplat\src\Shared\Shared.fs"
// #load @"c:\dev\code\graphplat\src\Server\Server.fs"


// content of Domain.fs - for whatever reason intellisens breaks when I include the original file
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

    type EventStoreEvent =
        | UserEvent of UserEvent
        | Event of Event

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

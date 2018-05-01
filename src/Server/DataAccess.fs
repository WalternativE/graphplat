module Events

open System
open Domain

type EventStore =
    { getUserEvents : unit -> Events.UserEvent
      getStream : Guid -> Events.Event list
      writeStreamedEvent : Guid -> Events.Event -> Events.Event
      writeUserEvent : Events.UserEvent -> Events.UserEvent }

module MartenStore =

    open Marten

    let eventStore (connectionString : string) =

        let store = DocumentStore.For connectionString

        let getUserEvents () =
            ()

        ()

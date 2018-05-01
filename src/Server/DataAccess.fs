namespace DataAccess

open System
open Domain

type EventStore =
    { getUserEvents : unit -> Events.UserEvent list
      getStream : Guid -> Events.Event list
      writeStreamedEvent : Guid -> Events.Event -> unit
      writeUserEvent : Events.UserEvent -> unit }

module MartenStore =

    open Marten
    open Domain.Events
    open Domain.EventStore

    let eventStore (connectionString : string) =

        let store = DocumentStore.For connectionString

        let getUserEvents () =
            use session = store.QuerySession ()
            session
            |> Session.query<StoreEvent<UserEvent>>
            |> Queryable.toList
            |> Seq.map (fun e -> e.Event)
            |> List.ofSeq

        let getStream (streamId : Guid) =
            use session = store.QuerySession ()
            session
            |> Session.query<StreamedStoreEvent<Event>>
            |> Queryable.filter <@ fun e -> e.StreamId = streamId @>
            |> Queryable.toList
            |> Seq.map (fun e -> e.Event)
            |> List.ofSeq

        let writeStreamedEvent (streamId : Guid) (event : Event) =
            use session = store.OpenSession ()
            let streamedEvent = { Id = Guid.NewGuid (); StreamId = streamId; Event = event }
            Session.storeSingle streamedEvent session
            Session.saveChanges session

        let writeUserEvent (userEvent : UserEvent) =
            use session = store.OpenSession ()
            let storeEvent = { Id = Guid.NewGuid (); Event = userEvent }
            Session.storeSingle storeEvent session
            Session.saveChanges session

        { getUserEvents = getUserEvents
          getStream = getStream
          writeStreamedEvent = writeStreamedEvent
          writeUserEvent = writeUserEvent }

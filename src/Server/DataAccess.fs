namespace DataAccess

open System

type EventStore<'a> =
    { getEvents : Guid -> 'a list
      writeEvent : Guid -> 'a -> unit }

module MartenStore =

    open Marten
    open Domain.EventStore

    let eventStore(connectionString : string) =

        let store = DocumentStore.For connectionString

        let getEvents (streamId : Guid) =
            use session = store.QuerySession ()
            session
            |> Session.query<StreamedStoreEvent<'a>>
            |> Queryable.filter <@ fun e -> e.StreamId = streamId @>
            |> Queryable.toList
            |> Seq.map (fun e -> e.Event)
            |> List.ofSeq

        let writeStreamedEvent (streamId : Guid) event =
            use session = store.OpenSession ()
            let streamedEvent = { Id = Guid.NewGuid (); StreamId = streamId; Event = event }
            Session.storeSingle streamedEvent session
            Session.saveChanges session

        { getEvents = getEvents
          writeEvent = writeStreamedEvent }

// #load @"references.fsx"
#load @"referencesnetstd.fsx"

type User =
    { Id : string
      Name : string}

type Event<'T> =
    { Id : string
      Payload : 'T }

type UserEvent =
    | UserCreatedEvent of string * string
    | UserUpdatedEvent of string * string

type Command<'T> =
    { Id : string
      Payload : string }

type UserCommand =
    | CreateUserCommand of Command<string * string>

open Marten

let connString = "Host=localhost;Port=5432;Username=postgres;Password=admin123;Database=graphplat"

let store = DocumentStore.For connString
let session = store.LightweightSession ()

open System

let guidString () = Guid.NewGuid() |> string

let uce : Event<UserEvent> = { Id = guidString (); Payload = UserCreatedEvent (guidString(), "Bob") }

session.Store(uce)
session.SaveChanges()

let oSession = store.OpenSession()
let existing = oSession.Query<UserEvent>()

existing
|> Seq.iter (fun s -> printfn "%A" s)

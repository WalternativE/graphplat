#load @"referencesnetstd.fsx"
// #load @"../Shared/Domain.fs"
#load @"DataAccess.fs"

open System
open Marten
open Domain.Model
open Domain.Events
open Domain.EventStore
open DataAccess

let connString = "Host=localhost;Port=5432;Username=postgres;Password=admin123;Database=graphplat"
let da = MartenStore.eventStore connString

let guid1 = Guid.Parse "84b197b8-fa66-4aac-a727-9446db4522ab"
let guid2 = Guid.Parse "84b197b8-fa66-4aac-a722-9446db4522ab"

let userEvents = da.getUserEvents ()
let stream = da.getStream guid2

let aUser = { Id = Guid.NewGuid (); Email = "gregor.beyerle@gmail.com" }
let aWorkspace = { Id = Guid.NewGuid (); Name = "Test workspace" }
let aUserSpace = { User = aUser; Workspaces = [] }

UserSpaceCreated aUserSpace
|> da.writeStreamedEvent guid2

let stream = da.getStream guid2

UserCreated aUser
|> da.writeUserEvent

let userEvents = da.getUserEvents ()

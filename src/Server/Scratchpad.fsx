#load @"referencesnetstd.fsx"
#load @"../Shared/Domain.fs"
#load @"DataAccess.fs"

open System
open Marten

let connString = "Host=localhost;Port=5432;Username=postgres;Password=admin123;Database=graphplat"

let store = DocumentStore.For connString
let session = store.LightweightSession ()

open Marten.Session

// storeSingle user session
// saveChanges session

// let res =
//     session
//     |> query<User>
//     |> Queryable.filter <@ fun e -> e.Name = "gregor.beyerle@gmail.com" @>
//     |> Queryable.head

// type Workspace =
//     { Id : Guid
//       Description : string
//       CreatedAt : DateTime }

// let nWs =
//     { Id = Guid.NewGuid ()
//       Description = "A test workspace"
//       CreatedAt = DateTime.UtcNow }
// let wsGuid = Guid.Parse "99db821e-9afa-4484-bdb1-0cef74dff97f"

// storeSingle nWs session
// saveChanges session

// let ws = loadByGuid<Workspace> wsGuid session

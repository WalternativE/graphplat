#load @"referencesnetstd.fsx"
#load "Shared/Domain.fs"

open System
open Domain
open Domain.Model
open Domain.Commands

let userGuid = Guid.Parse "33541660-08a9-4faa-b44f-65d2e23294be"

let user =
    { User.Id = userGuid
      User.Email = "gregor@test.com"
      User.Name = "Gregor" }

let createUserSapceCmd = CreateUserSpace user

let ev = Behaviour.handleCommand [] createUserSapceCmd

let ws =
    { Workspace.Id = Guid.NewGuid ()
      Workspace.Name = "A test workspace" }
let createWorksSpaceCmd = CreateWorkSpace ws

let ev2 = Behaviour.handleCommand [ ev ] createWorksSpaceCmd

let events = [ ev; ev2 ]
let initUs = Domain.Projection.initialUserSpace events
let currentUs = Domain.Projection.userSpacesState events initUs.Value

let duplicateWs = 
    { Workspace.Id = Guid.NewGuid ()
      Workspace.Name = "A test workspace" }
let cwsCmd = CreateWorkSpace duplicateWs

currentUs.Workspaces
|> List.exists (fun ws -> ws.Name = duplicateWs.Name)

let ev3 = Behaviour.handleCommand events cwsCmd
let events2 = events@[ev3]

let ws2 =
    { duplicateWs with Name = "A unique name" }

let ev4 =
    CreateWorkSpace ws2
    |> Behaviour.handleCommand events2
let events3 = events@[ev4]

let currentUs2 = Domain.Projection.userSpacesState events3 initUs.Value

let ev5 =
    DeleteWorkSpace ws2
    |> Behaviour.handleCommand events3

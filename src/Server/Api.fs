module Api

open System
open System.IO
open System.Security.Claims
open Microsoft.AspNetCore.Http

open Giraffe
open Saturn

open Auth
open Shared
open Domain.Queries
open Domain.Commands
open Domain.Model
open Domain.Events
open DataAccess

let getUserId (ctx : HttpContext) =
    let idClaim = ctx.User.FindFirst ClaimTypes.NameIdentifier
    Guid.Parse idClaim.Value

let getUser (ctx : HttpContext) =
    let name = ctx.User.FindFirst ClaimTypes.GivenName
    let uid = getUserId ctx
    let mail = ctx.User.FindFirst ClaimTypes.Email
    { Id = uid ; Name = name.Value; Email = mail.Value }

let handlePostToken (nxt : HttpFunc) (ctx : HttpContext) = task {
    let! model = ctx.BindJsonAsync<LoginCredentials>()

    let user = authenticateUser model
    match user with
    | Some u ->
        let tokenResult = { Token = generateToken u }
        return! json tokenResult nxt ctx
    | None ->
        let resp = setStatusCode 401 >=> text "Bad credentials"
        return! resp nxt ctx }

let getCurrentUser next (ctx : HttpContext) = task {
    let user = getUser ctx
    return! json user next ctx
}

[<Literal>]
let connectionString = "Host=localhost;Port=5432;Username=postgres;Password=admin123;Database=graphplat"
let spacesEventStore : EventStore<Event> = MartenStore.eventStore connectionString
let queryHandler = CQHandlers.spacesQueryHandler spacesEventStore

let getUserSpace (nxt : HttpFunc) (ctx : HttpContext) = task {
    let uid = getUserId ctx
    let space =
        GetUserspace uid
        |> queryHandler

    match space with
    | None ->
        let resp = setStatusCode 404 >=> text "Not found"
        return! resp nxt ctx
    | Some usp ->
        return! json usp nxt ctx
}

let commandHandler = CQHandlers.commandHandler spacesEventStore

let createUserSpace (nxt : HttpFunc) (ctx : HttpContext) = task {
    let user = getUser ctx
    let ev =
        CreateUserSpace user
        |> commandHandler

    match ev with
    | UserSpaceCreated usp ->
        let resp = setStatusCode 201 >=> json usp
        return! resp nxt ctx
    | Error e ->
        let resp = setStatusCode 400 >=> text (errorToString e)
        return! resp nxt ctx
}

let securedApiRouter = scope {
    pipe_through (Auth.requireAuthentication JWT)
    get "/users/current" getCurrentUser
    get "/users/current/user-space" getUserSpace
    post "/users/current/user-space" createUserSpace }

let apiRouter = scope {
    pipe_through (pipeline { set_header "x-pipeline-type" "Api" })
    post "/token" handlePostToken
    forward "/secured" securedApiRouter }

let clientPath = Path.Combine("..","Client") |> Path.GetFullPath
let browserRouter = scope {
    get "/" (htmlFile (Path.Combine(clientPath, "/index.html"))) }

let mainRouter = scope {
    forward "" browserRouter
    forward "/api" apiRouter }

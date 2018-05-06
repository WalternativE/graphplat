module ApiClient

open Shared
open Global

open Domain.Model

open System
open Fable.Core.JsInterop
open Fable.PowerPack
open Fable.PowerPack.Fetch

let authUser (creds : LoginCredentials) = promise {
    if String.IsNullOrWhiteSpace creds.Email then failwith "Please supply email"
    if String.IsNullOrWhiteSpace creds.Password then failwith "Please supply password"

    let body = toJson creds

    let props =
        [ RequestProperties.Method HttpMethod.POST
          Fetch.requestHeaders
            [ HttpRequestHeaders.ContentType "application/json" ]
          RequestProperties.Body !^body ]

    try
        return! Fetch.fetchAs<TokenResult> "/api/token" props
    with _ ->
        return failwithf "Could not authenticate user."
}

let acquireUserData (tr : TokenResult) = promise {
    let props = standardGetProps tr.Token

    try
        let! user = Fetch.fetchAs<User> "/api/secured/users/current" props
        return { User = user; Token = tr.Token}
    with _ ->
        return failwithf "Final user auth lookup failed."
}

let loadUserSpace (token : JWT) = promise {
    let props = standardGetProps token

    try
        return! Fetch.fetchAs<UserSpace> "/api/secured/users/current/user-space" props
    with e ->
        return
            e.Message
            |> extractFetchError
            |> failwithf "%s"
}

let createUserSpace (token : JWT) = promise {
    let props = standardPostProps token

    try
        return! Fetch.fetchAs<UserSpace> "/api/secured/users/current/user-space" props
    with e ->
        return
            e.Message
            |> extractFetchError
            |> failwithf "%s"
}

let createWorkSpace (args : Workspace * JWT) = promise {
    let (workspace, token) = args

    let props =
        standardPostProps token
        |> addBody (toJson workspace)

    try
        return! Fetch.fetchAs<Workspace> "/api/secured/workspaces" props
    with e ->
        return
            e.Message
            |> extractFetchError
            |> failwithf "%s"
}

let deleteWorkspace (args : Guid * JWT) = promise {
    let (id, token) = args

    let props =
        standardPostProps token

    let url = sprintf "/api/secured/workspaces/%O" id
    try
        return! Fetch.fetch url props
    with e ->
        return e.Message
        |> extractFetchError
        |> failwithf "%s"
}

let getWorkspace (args : Guid * JWT) = promise {
    let (id, token) = args

    let props =
        standardGetProps token

    let url = sprintf "/api/secured/workspaces/%O" id
    try
        return! Fetch.fetchAs<Workspace> url props
    with e ->
        return e.Message
        |> extractFetchError
        |> failwithf "%s"
}

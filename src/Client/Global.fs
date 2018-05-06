module Global

open Domain.Model

open Fable.PowerPack
open Fable.PowerPack.Fetch.Fetch_types
open Fable.Core.JsInterop

type Page =
    | Login
    | Home
    | Workspace of string

type JWT = string

type UserData =
    { User : User
      Token : JWT }

let toHash = function
    | Login -> "#/login"
    | Home -> "#/home"
    | Workspace s -> sprintf "#/workspaces/%s" s

// quite hacky
let extractFetchError (msg : string) =
    msg.Split(' ')
    |> Array.head

let standardProps (method : HttpMethod) (token : JWT) =
    [ RequestProperties.Method method
      Fetch.requestHeaders
        [ HttpRequestHeaders.ContentType "application/json"
          HttpRequestHeaders.Authorization ("Bearer " + token) ] ]

let standardGetProps = standardProps HttpMethod.GET
let standardPostProps = standardProps HttpMethod.POST

let addBody (body : string) (properties : RequestProperties list) =
    [ yield RequestProperties.Body !^body
      yield! properties ]

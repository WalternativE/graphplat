module Global
open Domain.Model

type Page =
    | Login
    | Home

type JWT = string

type UserData =
    { User : User
      Token : JWT }

let toHash = function
    | Login -> "#/login"
    | Home -> "#/home"

// quite hacky
let extractFetchError (msg : string) =
    msg.Split(' ')
    |> Array.head

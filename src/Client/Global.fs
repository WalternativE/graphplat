module Global

type Page =
    | Login
    | Home

type JWT = string

type UserData =
    { UserName : string
      Token : JWT }

let toHash = function
    | Login -> "#/login"
    | Home -> "#/home"

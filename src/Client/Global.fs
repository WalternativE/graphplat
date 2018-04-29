module Global

type Page =
    | Login
    | Home

type PageModel =
    | LoginPageModel
    | HomePageModel

type JWT = string

type UserData =
    { UserName : string
      Token : JWT }

type Model =
    { User : UserData option
      PageModel : PageModel }

type Message =
    | StorageFailure of exn
    | Logout
    | LoggedIn of UserData
    | LoggedOut

let toHash = function
    | Login -> "#/login"
    | Home -> "#/home"

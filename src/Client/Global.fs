module Global

type Username = string

type Page =
    | Login
    | Home

type Model =
    { User : Username option
      Page : Page }

type Message =
    | ChangePage of Page

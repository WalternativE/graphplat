namespace Shared

[<CLIMutable>]
type LoginCredentials =
    { Email : string
      Password : string }

[<CLIMutable>]
type TokenResult =
    { Token : string }

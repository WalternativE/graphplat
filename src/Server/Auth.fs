module Auth

open System
open System.IdentityModel.Tokens.Jwt
open Microsoft.IdentityModel.Tokens
open System.Security.Claims

open Shared
open Domain.Model

let issuer = "127.0.0.1"
let secret = "reallyReallySecretKey"

let generateToken (user : User) =
    let claims =
        [| Claim(JwtRegisteredClaimNames.Sub, user.Id.ToString())
           Claim(JwtRegisteredClaimNames.GivenName, user.Name)
           Claim(JwtRegisteredClaimNames.Email, user.Email)
           Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) |]
    claims
    |> Saturn.Auth.generateJWT (secret, SecurityAlgorithms.HmacSha256) issuer (DateTime.UtcNow.AddDays(1.0))

let authenticateUser (creds : LoginCredentials) : User option =
    // TODO right now there is only one user and it is hardcoded - this should not be the case
    if creds.Email = "gregor.beyerle@gmail.com" && creds.Password = "abc123!" then
        let user =
            { User.Id = Guid.Parse "43931771-a7e9-4c57-8188-ec3415f95dde"
              Email = creds.Email
              Name = "Gregor" }
        Some user
    else
        None

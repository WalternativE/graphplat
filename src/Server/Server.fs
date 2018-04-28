open System
open System.IO
open Microsoft.Extensions.DependencyInjection

open Giraffe
open Giraffe.Serialization
open Saturn

open Shared

open System.IdentityModel.Tokens.Jwt
open System.Security.Claims
open Microsoft.IdentityModel.Tokens
open Microsoft.AspNetCore.Http

let clientPath = Path.Combine("..","Client") |> Path.GetFullPath
let port = 8085us

let issuer = "127.0.0.1"
let secret = "reallyReallySecretKey"

let generateToken (email : string) =
    let claims =
        [| Claim(JwtRegisteredClaimNames.Sub, email)
           Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) |]
    claims
    |> Auth.generateJWT (secret, SecurityAlgorithms.HmacSha256) issuer (DateTime.UtcNow.AddDays(1.0))

let handlePostToken (nxt : HttpFunc) (ctx : HttpContext) = task {
    let! model = ctx.BindJsonAsync<LoginCredentials>()
    let tokenResult = { Token = generateToken model.Email }
    return! json tokenResult nxt ctx }

let getInitCounter nxt ctx = task {
    return! text "42" nxt ctx }

let getTestResponse next (ctx : HttpContext ) = task {
    let mail = ctx.User.FindFirst ClaimTypes.NameIdentifier
    let message =  sprintf "Hello, %s!" mail.Value
    return! text message next ctx }

let apiPipe = pipeline {
    set_header "x-pipeline-type" "Api" }

let securedApiRouter = scope {
    pipe_through (Auth.requireAuthentication JWT)
    get "/test" getTestResponse }

let apiRouter = scope {
    pipe_through apiPipe

    post "/token" handlePostToken

    get "/init" getInitCounter
    forward "/secured" securedApiRouter }

let browserRouter = scope {
    get "/" (htmlFile (Path.Combine(clientPath, "/index.html"))) }

let mainRouter = scope {
    forward "" browserRouter
    forward "/api" apiRouter }

let configureServices (services : IServiceCollection) =
    let fableJsonSettings = Newtonsoft.Json.JsonSerializerSettings()
    fableJsonSettings.Converters.Add(Fable.JsonConverter())
    services.AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer fableJsonSettings) |> ignore
    services

let app = application {
    router mainRouter
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_jwt_authentication secret issuer
    memory_cache
    use_static clientPath
    service_config configureServices
    use_gzip }

run app

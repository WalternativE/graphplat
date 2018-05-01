open Microsoft.Extensions.DependencyInjection

open Giraffe
open Giraffe.Serialization
open Saturn

open Auth
open Api

let port = 8085us

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

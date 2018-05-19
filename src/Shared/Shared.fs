namespace Shared

open Domain.Model

[<CLIMutable>]
type LoginCredentials =
    { Email : string
      Password : string }

[<CLIMutable>]
type TokenResult =
    { Token : string }

type WorkflowComputationOutput =
    | Unit
    | Scalar of double
    | Graph of SimpleGraph
    | Error of string

type LabeledOutput =
    { Label : OutputLabel
      Output : WorkflowComputationOutput }

module Login

open Fulma

module R = Fable.Helpers.React

let view =
    Section.section []
        [ Container.container []
            [ R.form []
                [ Field.div []
                    [ Label.label []
                        [ R.str "Email" ]
                      Control.div []
                        [ Input.email [ Input.Placeholder "bob@bobbing.com" ] ] ]
                  Field.div []
                    [ Label.label []
                        [ R.str "Password" ]
                      Control.div []
                        [ Input.password [] ] ]
                  Field.div [ Field.IsGrouped ]
                    [ Control.div []
                        [ Button.button [ Button.Color IsPrimary ]
                            [ R.str "Submit" ] ]
                      Control.div []
                        [ Button.button [ Button.Color IsWhite ]
                            [ R.str "Cancel" ] ] ] ] ] ]

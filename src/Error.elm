module Error exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA


byFieldName : String -> Dict String (List String) -> List String
byFieldName field errors =
    errors
        |> Dict.get field
        |> Maybe.withDefault []


{-| Returns True if has error by given field
-}
hasError : String -> Dict String (List String) -> Bool
hasError field errors =
    byFieldName field errors
        |> List.length
        |> (/=) 0


{-| For form fields that are not InputField element,
but we want it to have same errors UI
-}
withStandaloneField : List String -> Html msg
withStandaloneField lst =
    if List.isEmpty lst then
        Html.text ""

    else
        Html.div [ HA.class "mt-1" ]
            (List.map
                (\error -> viewError [ error ])
                lst
            )


viewError : List String -> Html msg
viewError errors =
    Html.div
        [ HA.class "bg-red-100 border border-red-400 text-red-700 px-2 py-1 rounded relative" ]
        (List.map
            (\error ->
                Html.span
                    [ HA.class "text-red-500 text-sm" ]
                    [ Html.text error ]
            )
            errors
        )

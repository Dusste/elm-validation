module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Error
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Task
import Validation


textareaId =
    "textarea_id"


attachmentId =
    "attachment_id"


textInputId =
    "text_input_id"


choosableFieldId =
    "choosable_field_id"



-- MODEL


type alias Attachment =
    { file : Maybe String
    , name : String
    }


type alias User =
    { name : String
    , email : String
    , role : String
    }


type alias Model =
    { formErrors : Dict String (List String)
    , textareaValue : String
    , attachment : Maybe Attachment
    , textInputValue : String
    , choosableValue : String
    , users : List User
    , selectedUser : Maybe User
    , searchUsersValue : String
    , isUsersDropdownOpen : Bool
    }


initialModel : Model
initialModel =
    { formErrors = Dict.empty
    , textareaValue = ""
    , attachment = Nothing
    , textInputValue = ""
    , choosableValue = ""
    , users =
        [ { name = "John Johnson", email = "john.doe@example.com", role = "Admin" }
        , { name = "Tim Markus", email = "tim.markus@something.com", role = "Member" }
        , { name = "Chris Green", email = "chris.green@lala.com", role = "Member" }
        ]
    , selectedUser = Nothing
    , searchUsersValue = ""
    , isUsersDropdownOpen = False
    }



-- MESSAGES


type Msg
    = Submit
    | SetTextareaValue String
    | TriggerAttachFile String
    | AttachFileRequestProceed String File
    | AttachFileRead String String (Result Http.Error String)
    | SetTextInputValue String
    | SetChoosableValue String
    | SetSearchUsersValue String
    | SelectUser User



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTextInputValue value ->
            let
                resetErrorsPerField : Dict String (List String)
                resetErrorsPerField =
                    Validation.resetErrorsPerField textInputId model.formErrors
            in
            ( { model | textInputValue = value, formErrors = resetErrorsPerField }
            , Cmd.none
            )

        SetTextareaValue value ->
            let
                resetErrorsPerField : Dict String (List String)
                resetErrorsPerField =
                    Validation.resetErrorsPerField textareaId model.formErrors
            in
            ( { model | textareaValue = value, formErrors = resetErrorsPerField }
            , Cmd.none
            )

        SetChoosableValue value ->
            let
                resetErrorsPerField : Dict String (List String)
                resetErrorsPerField =
                    Validation.resetErrorsPerField choosableFieldId model.formErrors
            in
            ( { model | choosableValue = value, formErrors = resetErrorsPerField }
            , Cmd.none
            )

        Submit ->
            let
                validationConfig : Validation.Config
                validationConfig =
                    { validationRules =
                        [ { fieldName = textareaId
                          , fieldRules =
                                [ Validation.CheckStringTooShort 20
                                , Validation.CheckStringTooLong 200
                                ]
                          , fieldValue = model.textareaValue
                          }
                        , { fieldName = attachmentId
                          , fieldRules =
                                [ Validation.CheckFileSize 300000 -- 300kb
                                , Validation.CheckFileType [ "pdf", "excel", "doc", "docx", "jpg", "jpeg", "png" ] -- allowed file types
                                ]
                          , fieldValue = model.attachment |> Maybe.andThen .file |> Maybe.withDefault ""
                          }
                        , { fieldName = textInputId
                          , fieldRules =
                                [ Validation.CheckEmptyEmail
                                , Validation.CheckInvalidEmail
                                ]
                          , fieldValue = model.textInputValue
                          }
                        , { fieldName = choosableFieldId
                          , fieldRules =
                                [ Validation.InvalidChoosableField ]
                          , fieldValue = model.choosableValue
                          }
                        , { fieldName = ""
                          , fieldRules =
                                [ Validation.CheckShouldMatch
                                    (model.users |> List.map (\user -> user.name))
                                ]
                          , fieldValue = model.selectedUser |> Maybe.map .name |> Maybe.withDefault ""
                          }
                        ]
                    , initialErrors = model.formErrors
                    }

                potentialErrors : Dict String (List String)
                potentialErrors =
                    Validation.checkErrors validationConfig
            in
            if Validation.anyActiveError potentialErrors then
                ( { model | formErrors = potentialErrors }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        TriggerAttachFile id ->
            let
                resetErrorsPerField : Dict String (List String)
                resetErrorsPerField =
                    Validation.resetErrorsPerField id model.formErrors
            in
            ( { model | formErrors = resetErrorsPerField }
            , Select.file
                -- TODO check this list of file types, maybe its not correct -> we need: "pdf", "excel", "Word", "spreadsheet", "docs", "png", "jpg"
                [ "text/pdf"
                , "application/pdf"
                , "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
                , "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                , "application/vnd.openxmlformats-officedocument.presentationml.presentation"
                , "application/msword"
                , "application/vnd.ms-excel"
                , "application/vnd.ms-powerpoint"
                , "application/vnd.ms-office"
                , "image/png"
                , "image/jpeg"
                ]
                (AttachFileRequestProceed id)
            )

        AttachFileRequestProceed key file ->
            ( model
            , Task.attempt (AttachFileRead key (File.name file)) (File.toUrl file)
            )

        AttachFileRead id fileName result ->
            case result of
                Ok fileToUrl ->
                    let
                        updateAttachment : Attachment
                        updateAttachment =
                            { file = Just fileToUrl
                            , name = fileName
                            }
                    in
                    ( { model | attachment = Just updateAttachment }
                    , Cmd.none
                    )

                Err error ->
                    ( model
                    , Cmd.none
                    )

        SetSearchUsersValue value ->
            let
                resetErrorsPerField : Dict String (List String)
                resetErrorsPerField =
                    Validation.resetErrorsPerField "" model.formErrors
            in
            ( { model
                | searchUsersValue = value
                , isUsersDropdownOpen = True
                , formErrors = resetErrorsPerField
                , selectedUser = Nothing
              }
            , Cmd.none
            )

        SelectUser user ->
            ( { model
                | selectedUser = Just user
                , searchUsersValue = user.name
                , isUsersDropdownOpen = False
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ HA.class "container flex flex-col mx-auto max-w-2xl gap-4 my-10" ]
        [ Html.h1
            [ HA.class "text-center text-4xl m-2 text-cyan-400" ]
            [ Html.text "Elm Validation" ]
        , Html.hr [ HA.class "my-4" ] []
        , Html.div
            [ HA.class "mt-2 flex flex-col gap-2" ]
            [ Html.h2
                [ HA.class "text-xl font-bold" ]
                [ Html.text "Minimum and maximum value checks" ]
            , Html.textarea
                [ HA.id textareaId
                , HA.class "w-full p-2 rounded focus:outline-none focus:ring-0 focus:border-gray-400 min-h-[100px]"
                , HA.style "border"
                    (if Error.hasError textareaId model.formErrors then
                        "1px solid red"

                     else
                        "1px solid #ccc"
                    )
                , HA.placeholder "Enter your text here"
                , HA.value model.textareaValue
                , HE.onInput SetTextareaValue
                ]
                []
            , Error.byFieldName textareaId model.formErrors
                |> Error.withStandaloneField
            ]
        , Html.hr [ HA.class "my-4" ] []
        , Html.div
            [ HA.class "mt-2 flex flex-col gap-2" ]
            [ Html.h2
                [ HA.class "text-xl font-bold" ]
                [ Html.text "File size and type checks" ]
            , Html.button
                [ HA.type_ "button"
                , HA.class "bg-gray-200 border border-gray-300 text-gray-800 px-4 py-2 rounded hover:bg-gray-300"
                , HE.onClick (TriggerAttachFile attachmentId)
                ]
                [ Html.text "Attach File" ]
            , case model.attachment of
                Just attachment ->
                    Html.span
                        [ HA.class "text-sm text-gray-500" ]
                        [ Html.text ("Attached file: " ++ attachment.name) ]

                Nothing ->
                    Html.text ""
            , Error.byFieldName attachmentId model.formErrors
                |> Error.withStandaloneField
            ]
        , Html.hr [ HA.class "my-4" ] []
        , Html.div
            [ HA.class "mt-2 flex flex-col gap-2" ]
            [ Html.h2
                [ HA.class "text-xl font-bold" ]
                [ Html.text "Email validation" ]
            , Html.input
                [ HA.type_ "text"
                , HA.class "w-full p-2 rounded focus:outline-none focus:ring-0 focus:border-gray-400"
                , HA.style "border"
                    (if Error.hasError textInputId model.formErrors then
                        "1px solid red"

                     else
                        "1px solid #ccc"
                    )
                , HA.placeholder "Enter your email here"
                , HA.value model.textInputValue
                , HE.onInput SetTextInputValue
                ]
                []
            , Error.byFieldName textInputId model.formErrors
                |> Error.withStandaloneField
            ]
        , Html.hr [ HA.class "my-4" ] []
        , Html.div
            [ HA.class "mt-2 flex flex-col gap-2" ]
            [ Html.h2
                [ HA.class "text-xl font-bold" ]
                [ Html.text "Choosable field validation" ]
            , Html.select
                [ HA.id choosableFieldId
                , HA.class "w-full p-2 rounded focus:outline-none focus:ring-0 focus:border-gray-400"
                , HA.style "border"
                    (if Error.hasError choosableFieldId model.formErrors then
                        "1px solid red"

                     else
                        "1px solid #ccc"
                    )
                , HA.placeholder "Select an option"
                , HA.value model.choosableValue
                , HE.onInput SetChoosableValue
                ]
                [ Html.option [ HA.value "" ] [ Html.text "Select an option" ]
                , Html.option [ HA.value "option1" ] [ Html.text "Option 1" ]
                , Html.option [ HA.value "option2" ] [ Html.text "Option 2" ]
                , Html.option [ HA.value "option3" ] [ Html.text "Option 3" ]
                ]
            , Error.byFieldName choosableFieldId model.formErrors
                |> Error.withStandaloneField
            ]
        , Html.hr [ HA.class "my-4" ] []
        , Html.div
            [ HA.class "mt-2 flex flex-col gap-2" ]
            [ Html.h2
                [ HA.class "text-xl font-bold" ]
                [ Html.text "Value should match one of the options from the list" ]
            , Html.p
                [ HA.class "text-sm text-gray-500" ]
                [ Html.text "Select a user from the list below" ]
            , case model.selectedUser of
                Just user ->
                    Html.div
                        [ HA.class "bg-gray-100 p-2 rounded-md border border-gray-200" ]
                        [ Html.p [ HA.class "text-sm font-semibold" ] [ Html.text user.name ]
                        , Html.p [ HA.class "text-sm text-gray-500" ] [ Html.text user.email ]
                        , Html.p [ HA.class "text-sm text-red-800" ] [ Html.text user.role ]
                        ]

                Nothing ->
                    Html.text ""
            , Html.input
                [ HA.type_ "text"
                , HA.class "w-full p-2 rounded focus:outline-none focus:ring-0 focus:border-gray-400"
                , HA.style "border"
                    (if Error.hasError "" model.formErrors then
                        "1px solid red"

                     else
                        "1px solid #ccc"
                    )
                , HA.placeholder "Search users"
                , HA.value model.searchUsersValue
                , HE.onInput SetSearchUsersValue
                ]
                []
            , if model.searchUsersValue /= "" && model.isUsersDropdownOpen then
                Html.ul
                    [ HA.class "rounded-md border bg-white border-gray-300 focus:outline-none focus:ring-0 focus:border-gray-400 overflow-hidden" ]
                    (model.users
                        |> List.filter (\user -> String.contains (String.toLower model.searchUsersValue) (String.toLower user.name) || String.contains (String.toLower model.searchUsersValue) (String.toLower user.email))
                        |> (\lst ->
                                if List.length lst > 0 then
                                    lst

                                else
                                    [ { name = "No users found", email = "", role = "" } ]
                           )
                        |> List.map
                            (\user ->
                                Html.li
                                    [ HA.class "text-sm border-t border-gray-200 text-gray-500 cursor-pointer hover:bg-gray-100 p-2 first:border-t-0"
                                    , HE.onClick (SelectUser user)
                                    ]
                                    [ Html.p [ HA.class "text-sm font-semibold" ] [ Html.text user.name ]
                                    , Html.p [ HA.class "text-sm text-gray-500" ] [ Html.text user.email ]
                                    , Html.p [ HA.class "text-sm text-red-800" ] [ Html.text user.role ]
                                    ]
                            )
                    )

              else
                Html.text ""
            , Error.byFieldName "" model.formErrors
                |> Error.withStandaloneField
            ]
        , Html.button
            [ HA.type_ "button"
            , HA.class "bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600"
            , HE.onClick Submit
            ]
            [ Html.text "Submit" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

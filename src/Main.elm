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
import Time
import Validation


textareaId =
    "textarea_id"


attachmentId =
    "attachment_id"


textInputId =
    "text_input_id"


choosableFieldId =
    "choosable_field_id"


passwordId =
    "password_id"


confirmPasswordId =
    "confirm_password_id"


businessEmailId =
    "business_email_id"


fullNameId =
    "full_name_id"


phoneId =
    "phone_id"


cardId =
    "card_id"


cvcId =
    "cvc_id"


expiryId =
    "expiry_id"


alphaOnlyId =
    "alpha_only_id"


slugId =
    "slug_id"


skillsFieldId =
    "skills_field_id"


companyId =
    "company_id"


requiredNoteId =
    "required_note_id"



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
    , zone : Maybe Time.Zone
    , now : Maybe Time.Posix
    , password : String
    , confirmPassword : String
    , businessEmail : String
    , fullName : String
    , phone : String
    , cardNumber : String
    , cvc : String
    , expiry : String
    , alphaOnly : String
    , slug : String
    , selectedSkills : List String
    , company : String
    , requiredNote : String
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
    , zone = Nothing
    , now = Nothing
    , password = ""
    , confirmPassword = ""
    , businessEmail = ""
    , fullName = ""
    , phone = ""
    , cardNumber = ""
    , cvc = ""
    , expiry = ""
    , alphaOnly = ""
    , slug = ""
    , selectedSkills = []
    , company = ""
    , requiredNote = ""
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
    | Tick Time.Posix
    | GotClock ( Time.Zone, Time.Posix )
    | SetPassword String
    | SetConfirmPassword String
    | SetBusinessEmail String
    | SetFullName String
    | SetPhone String
    | SetCardNumber String
    | SetCvc String
    | SetExpiry String
    | SetAlphaOnly String
    | SetSlug String
    | ToggleSkill String
    | SetCompany String
    | SetRequiredNote String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotClock ( zone, posix ) ->
            ( { model | zone = Just zone, now = Just posix }, Cmd.none )

        Tick posix ->
            ( { model | now = Just posix }, Cmd.none )

        SetPassword value ->
            ( { model
                | password = value
                , formErrors = Validation.resetErrorsPerField passwordId model.formErrors
              }
            , Cmd.none
            )

        SetConfirmPassword value ->
            ( { model
                | confirmPassword = value
                , formErrors = Validation.resetErrorsPerField confirmPasswordId model.formErrors
              }
            , Cmd.none
            )

        SetBusinessEmail value ->
            ( { model
                | businessEmail = value
                , formErrors = Validation.resetErrorsPerField businessEmailId model.formErrors
              }
            , Cmd.none
            )

        SetFullName value ->
            ( { model
                | fullName = value
                , formErrors = Validation.resetErrorsPerField fullNameId model.formErrors
              }
            , Cmd.none
            )

        SetPhone value ->
            ( { model
                | phone = value
                , formErrors = Validation.resetErrorsPerField phoneId model.formErrors
              }
            , Cmd.none
            )

        SetCardNumber value ->
            ( { model
                | cardNumber = value
                , formErrors = Validation.resetErrorsPerField cardId model.formErrors
              }
            , Cmd.none
            )

        SetCvc value ->
            ( { model
                | cvc = value
                , formErrors = Validation.resetErrorsPerField cvcId model.formErrors
              }
            , Cmd.none
            )

        SetExpiry value ->
            ( { model
                | expiry = value
                , formErrors = Validation.resetErrorsPerField expiryId model.formErrors
              }
            , Cmd.none
            )

        SetAlphaOnly value ->
            ( { model
                | alphaOnly = value
                , formErrors = Validation.resetErrorsPerField alphaOnlyId model.formErrors
              }
            , Cmd.none
            )

        SetSlug value ->
            ( { model
                | slug = value
                , formErrors = Validation.resetErrorsPerField slugId model.formErrors
              }
            , Cmd.none
            )

        ToggleSkill skill ->
            let
                nextSkills =
                    if List.member skill model.selectedSkills then
                        List.filter (\s -> s /= skill) model.selectedSkills

                    else
                        skill :: model.selectedSkills
            in
            ( { model
                | selectedSkills = nextSkills
                , formErrors = Validation.resetErrorsPerField skillsFieldId model.formErrors
              }
            , Cmd.none
            )

        SetCompany value ->
            ( { model
                | company = value
                , formErrors = Validation.resetErrorsPerField companyId model.formErrors
              }
            , Cmd.none
            )

        SetRequiredNote value ->
            ( { model
                | requiredNote = value
                , formErrors = Validation.resetErrorsPerField requiredNoteId model.formErrors
              }
            , Cmd.none
            )

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
                expiryRules : List (String -> Validation.Validation)
                expiryRules =
                    case ( model.zone, model.now ) of
                        ( Just z, Just n ) ->
                            [ Validation.CheckEmptyExparation
                            , \s -> Validation.CheckExparation z n s
                            ]

                        _ ->
                            [ Validation.CheckEmptyExparation ]

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
                        , { fieldName = passwordId
                          , fieldRules =
                                [ Validation.CheckEmptyPassword
                                , Validation.CheckPasswordTooShort 10
                                , Validation.CheckPasswordCapitalize
                                , Validation.CheckPasswordSpecialChar
                                , Validation.CheckPasswordContainsInt
                                ]
                          , fieldValue = model.password
                          }
                        , { fieldName = confirmPasswordId
                          , fieldRules =
                                [ \confirm -> Validation.CheckPasswordMatch model.password confirm ]
                          , fieldValue = model.confirmPassword
                          }
                        , { fieldName = businessEmailId
                          , fieldRules =
                                [ Validation.CheckEmptyEmail
                                , Validation.CheckInvalidEmail
                                , Validation.CheckForBusiness
                                ]
                          , fieldValue = model.businessEmail
                          }
                        , { fieldName = fullNameId
                          , fieldRules =
                                [ Validation.CheckEmptyName
                                , Validation.CheckName
                                ]
                          , fieldValue = model.fullName
                          }
                        , { fieldName = phoneId
                          , fieldRules =
                                [ Validation.CheckEmptyPhoneNumber
                                , Validation.CheckPhoneNumber
                                ]
                          , fieldValue = model.phone
                          }
                        , { fieldName = cardId
                          , fieldRules =
                                [ Validation.CheckEmptyCard
                                , Validation.CheckCard
                                ]
                          , fieldValue = model.cardNumber
                          }
                        , { fieldName = cvcId
                          , fieldRules =
                                [ Validation.CheckEmptyCvc
                                , Validation.CheckCvcLength
                                ]
                          , fieldValue = model.cvc
                          }
                        , { fieldName = expiryId
                          , fieldRules = expiryRules
                          , fieldValue = model.expiry
                          }
                        , { fieldName = alphaOnlyId
                          , fieldRules =
                                [ Validation.CheckForIntInInput
                                , Validation.SpecialCharacter
                                ]
                          , fieldValue = model.alphaOnly
                          }
                        , { fieldName = slugId
                          , fieldRules =
                                [ Validation.CheckInvalidField
                                , Validation.CheckForDuplicate [ "admin", "api", "www" ]
                                ]
                          , fieldValue = model.slug
                          }
                        , { fieldName = skillsFieldId
                          , fieldRules =
                                [ \_ ->
                                    Validation.CheckIfListHaveMinimumItems model.selectedSkills 2 ""
                                ]
                          , fieldValue = ""
                          }
                        , { fieldName = companyId
                          , fieldRules =
                                [ Validation.CheckEmptyCompany ]
                          , fieldValue = model.company
                          }
                        , { fieldName = requiredNoteId
                          , fieldRules =
                                [ Validation.CheckInvalidField ]
                          , fieldValue = model.requiredNote
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

        AttachFileRead _ fileName result ->
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

                Err _ ->
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


skillOptions : List String
skillOptions =
    [ "Elm", "TypeScript", "Rust", "Python" ]


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
                [ Html.text "Business email (no common personal domains)" ]
            , Html.p
                [ HA.class "text-sm text-gray-500" ]
                [ Html.text "Same shape as personal email checks, plus rejects gmail.com, outlook.com, etc." ]
            , Html.input
                [ HA.type_ "text"
                , HA.class "w-full p-2 rounded focus:outline-none focus:ring-0 focus:border-gray-400"
                , HA.style "border"
                    (if Error.hasError businessEmailId model.formErrors then
                        "1px solid red"

                     else
                        "1px solid #ccc"
                    )
                , HA.placeholder "you@company.com"
                , HA.value model.businessEmail
                , HE.onInput SetBusinessEmail
                ]
                []
            , Error.byFieldName businessEmailId model.formErrors
                |> Error.withStandaloneField
            ]
        , Html.hr [ HA.class "my-4" ] []
        , Html.div
            [ HA.class "mt-2 flex flex-col gap-2" ]
            [ Html.h2
                [ HA.class "text-xl font-bold" ]
                [ Html.text "Password rules + confirmation" ]
            , Html.input
                [ HA.type_ "password"
                , HA.class "w-full p-2 rounded focus:outline-none focus:ring-0 focus:border-gray-400"
                , HA.style "border"
                    (if Error.hasError passwordId model.formErrors then
                        "1px solid red"

                     else
                        "1px solid #ccc"
                    )
                , HA.placeholder "Password (10+ chars, cap, number, special)"
                , HA.value model.password
                , HE.onInput SetPassword
                ]
                []
            , Error.byFieldName passwordId model.formErrors
                |> Error.withStandaloneField
            , Html.input
                [ HA.type_ "password"
                , HA.class "w-full p-2 rounded focus:outline-none focus:ring-0 focus:border-gray-400 mt-2"
                , HA.style "border"
                    (if Error.hasError confirmPasswordId model.formErrors then
                        "1px solid red"

                     else
                        "1px solid #ccc"
                    )
                , HA.placeholder "Confirm password"
                , HA.value model.confirmPassword
                , HE.onInput SetConfirmPassword
                ]
                []
            , Error.byFieldName confirmPasswordId model.formErrors
                |> Error.withStandaloneField
            ]
        , Html.hr [ HA.class "my-4" ] []
        , Html.div
            [ HA.class "mt-2 flex flex-col gap-2" ]
            [ Html.h2
                [ HA.class "text-xl font-bold" ]
                [ Html.text "Full name (two alphabetic words)" ]
            , Html.input
                [ HA.type_ "text"
                , HA.class "w-full p-2 rounded focus:outline-none focus:ring-0 focus:border-gray-400"
                , HA.style "border"
                    (if Error.hasError fullNameId model.formErrors then
                        "1px solid red"

                     else
                        "1px solid #ccc"
                    )
                , HA.placeholder "Jane Doe"
                , HA.value model.fullName
                , HE.onInput SetFullName
                ]
                []
            , Error.byFieldName fullNameId model.formErrors
                |> Error.withStandaloneField
            ]
        , Html.hr [ HA.class "my-4" ] []
        , Html.div
            [ HA.class "mt-2 flex flex-col gap-2" ]
            [ Html.h2
                [ HA.class "text-xl font-bold" ]
                [ Html.text "Phone number" ]
            , Html.input
                [ HA.type_ "text"
                , HA.class "w-full p-2 rounded focus:outline-none focus:ring-0 focus:border-gray-400"
                , HA.style "border"
                    (if Error.hasError phoneId model.formErrors then
                        "1px solid red"

                     else
                        "1px solid #ccc"
                    )
                , HA.placeholder "+1 234 567 8901"
                , HA.value model.phone
                , HE.onInput SetPhone
                ]
                []
            , Error.byFieldName phoneId model.formErrors
                |> Error.withStandaloneField
            ]
        , Html.hr [ HA.class "my-4" ] []
        , Html.div
            [ HA.class "mt-2 flex flex-col gap-2" ]
            [ Html.h2
                [ HA.class "text-xl font-bold" ]
                [ Html.text "Card, CVC, expiration (MM/YY)" ]
            , Html.p
                [ HA.class "text-sm text-gray-500" ]
                [ Html.text "Use a test number that passes Luhn, e.g. 4242 4242 4242 4242." ]
            , Html.input
                [ HA.type_ "text"
                , HA.class "w-full p-2 rounded focus:outline-none focus:ring-0 focus:border-gray-400"
                , HA.style "border"
                    (if Error.hasError cardId model.formErrors then
                        "1px solid red"

                     else
                        "1px solid #ccc"
                    )
                , HA.placeholder "Card number"
                , HA.value model.cardNumber
                , HE.onInput SetCardNumber
                ]
                []
            , Error.byFieldName cardId model.formErrors
                |> Error.withStandaloneField
            , Html.div
                [ HA.class "flex gap-2 mt-2" ]
                [ Html.input
                    [ HA.type_ "text"
                    , HA.class "w-24 p-2 rounded focus:outline-none focus:ring-0 focus:border-gray-400"
                    , HA.style "border"
                        (if Error.hasError cvcId model.formErrors then
                            "1px solid red"

                         else
                            "1px solid #ccc"
                        )
                    , HA.placeholder "CVC"
                    , HA.value model.cvc
                    , HE.onInput SetCvc
                    ]
                    []
                , Html.input
                    [ HA.type_ "text"
                    , HA.class "flex-1 p-2 rounded focus:outline-none focus:ring-0 focus:border-gray-400"
                    , HA.style "border"
                        (if Error.hasError expiryId model.formErrors then
                            "1px solid red"

                         else
                            "1px solid #ccc"
                        )
                    , HA.placeholder "MM/YY"
                    , HA.value model.expiry
                    , HE.onInput SetExpiry
                    ]
                    []
                ]
            , Error.byFieldName cvcId model.formErrors
                |> Error.withStandaloneField
            , Error.byFieldName expiryId model.formErrors
                |> Error.withStandaloneField
            ]
        , Html.hr [ HA.class "my-4" ] []
        , Html.div
            [ HA.class "mt-2 flex flex-col gap-2" ]
            [ Html.h2
                [ HA.class "text-xl font-bold" ]
                [ Html.text "Letters only (no digits, no punctuation)" ]
            , Html.p
                [ HA.class "text-sm text-gray-500" ]
                [ Html.text "Combines CheckForIntInInput and SpecialCharacter on one field." ]
            , Html.input
                [ HA.type_ "text"
                , HA.class "w-full p-2 rounded focus:outline-none focus:ring-0 focus:border-gray-400"
                , HA.style "border"
                    (if Error.hasError alphaOnlyId model.formErrors then
                        "1px solid red"

                     else
                        "1px solid #ccc"
                    )
                , HA.placeholder "letters and spaces only"
                , HA.value model.alphaOnly
                , HE.onInput SetAlphaOnly
                ]
                []
            , Error.byFieldName alphaOnlyId model.formErrors
                |> Error.withStandaloneField
            ]
        , Html.hr [ HA.class "my-4" ] []
        , Html.div
            [ HA.class "mt-2 flex flex-col gap-2" ]
            [ Html.h2
                [ HA.class "text-xl font-bold" ]
                [ Html.text "Pick at least two skills (CheckIfListHaveMinimumItems)" ]
            , Html.p
                [ HA.class "text-sm text-gray-500" ]
                [ Html.text "The third argument to the variant is ignored; the list lives in your model." ]
            , Html.div
                [ HA.class "flex flex-wrap gap-3" ]
                (List.map
                    (\skill ->
                        Html.label
                            [ HA.class "inline-flex items-center gap-2 text-sm cursor-pointer" ]
                            [ Html.input
                                [ HA.type_ "checkbox"
                                , HA.checked (List.member skill model.selectedSkills)
                                , HE.onCheck (\_ -> ToggleSkill skill)
                                ]
                                []
                            , Html.text skill
                            ]
                    )
                    skillOptions
                )
            , Error.byFieldName skillsFieldId model.formErrors
                |> Error.withStandaloneField
            ]
        , Html.hr [ HA.class "my-4" ] []
        , Html.div
            [ HA.class "mt-2 flex flex-col gap-2" ]
            [ Html.h2
                [ HA.class "text-xl font-bold" ]
                [ Html.text "Required note (CheckInvalidField)" ]
            , Html.input
                [ HA.type_ "text"
                , HA.class "w-full p-2 rounded focus:outline-none focus:ring-0 focus:border-gray-400"
                , HA.style "border"
                    (if Error.hasError requiredNoteId model.formErrors then
                        "1px solid red"

                     else
                        "1px solid #ccc"
                    )
                , HA.placeholder "Any non-empty text"
                , HA.value model.requiredNote
                , HE.onInput SetRequiredNote
                ]
                []
            , Error.byFieldName requiredNoteId model.formErrors
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
    Time.every 60000 Tick



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( initialModel
                , Task.perform GotClock (Task.map2 Tuple.pair Time.here Time.now)
                )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

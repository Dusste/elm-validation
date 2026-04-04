module Validation exposing
    ( Config
    , Validation(..)
    , ValidationRule
    , anyActiveError
    , checkErrors
    , resetErrorsPerField
    , updateError
    )

import Dict exposing (Dict)
import Time
import Utils


type alias Config =
    { validationRules : List ValidationRule
    , initialErrors : Dict String (List String)
    }


type alias ValidationRule =
    { fieldName : String
    , fieldRules : List (String -> Validation)
    , fieldValue : String
    }


type Validation
    = CheckEmptyEmail String
    | CheckForIntInInput String
    | CheckInvalidEmail String
    | CheckForBusiness String
    | CheckEmptyName String
    | CheckEmptyCvc String
    | CheckCvcLength String
    | CheckEmptyExparation String
    | CheckExparation Time.Zone Time.Posix String
    | CheckEmptyPhoneNumber String
    | CheckFileSize Int String -- size in bytes
    | CheckFileType (List String) String -- allowed file types
    | CheckEmptyCard String
    | CheckCard String
    | CheckPhoneNumber String
    | CheckEmptyCompany String
    | CheckName String
    | CheckEmptyPassword String
    | CheckStringTooShort Int String
    | CheckStringTooLong Int String
    | CheckPasswordTooShort Int String
    | CheckPasswordCapitalize String
    | CheckPasswordSpecialChar String
    | CheckPasswordContainsInt String
    | CheckPasswordMatch String String
    | CheckInvalidField String -- most general check for missing any value
    | InvalidChoosableField String -- most general choosable field validation
    | CheckShouldMatch (List String) String -- string should match one of the strings from list
    | CheckForDuplicate (List String) String -- string should not be duplicate of any string from list
    | CheckIfListHaveMinimumItems (List String) Int String -- Ignore actual value that you are passing, used for ex. for `preselectedMultiselect` to check if anything was selected
    | SpecialCharacter String


validationToErrorMsg : Validation -> String
validationToErrorMsg validation =
    case validation of
        -- TODO more intuitive way to know what to print
        CheckInvalidField _ ->
            "Field can't be empty"

        CheckForIntInInput _ ->
            "Digits not allowed"

        SpecialCharacter _ ->
            "Special characters not allowed"

        InvalidChoosableField _ ->
            "Required"

        CheckEmptyEmail _ ->
            "Email is empty"

        CheckForBusiness _ ->
            "Not a business email"

        CheckEmptyName _ ->
            "Name is empty"

        CheckEmptyExparation _ ->
            "Expiration date is empty"

        CheckStringTooShort n _ ->
            "Minimum is " ++ String.fromInt n ++ " characters"

        CheckStringTooLong n _ ->
            "Maximum is " ++ String.fromInt n ++ " characters"

        CheckExparation _ _ _ ->
            "Format is invalid or expired"

        CheckEmptyCompany _ ->
            "Company name is empty"

        CheckFileSize size value ->
            "File size must be less than " ++ Utils.bytesToKilobytes size ++ ". Size of your file is: " ++ Utils.bytesToKilobytes (Utils.calculateFileSizeInBytes value)

        CheckFileType fileTypes value ->
            "File type must be one of the following: " ++ String.join ", " fileTypes

        CheckEmptyCvc _ ->
            "CVC is empty"

        CheckCvcLength _ ->
            "CVC should be 3 numbers"

        CheckEmptyCard _ ->
            "Card number is empty"

        CheckCard _ ->
            "Card number is invalid"

        CheckEmptyPhoneNumber _ ->
            "Phone number is empty"

        CheckPhoneNumber _ ->
            "Phone number is invalid"

        CheckName _ ->
            "Add name"

        CheckInvalidEmail _ ->
            "Email is invalid"

        CheckEmptyPassword _ ->
            "Password is empty"

        CheckPasswordTooShort _ _ ->
            "Password must have at least 10 characters"

        CheckPasswordCapitalize _ ->
            "Password must contain at least one capitalized letter"

        CheckPasswordContainsInt _ ->
            "Password must contain at least one number"

        CheckPasswordSpecialChar _ ->
            "Password must contain at least one special character"

        CheckPasswordMatch _ _ ->
            "Passwors doesn't match"

        CheckShouldMatch _ _ ->
            "Name not found"

        CheckForDuplicate _ _ ->
            "Only unique values allowed"

        CheckIfListHaveMinimumItems _ minItemCount _ ->
            "At least " ++ String.fromInt minItemCount ++ "  items are required"


checkErrors : Config -> Dict String (List String)
checkErrors { validationRules, initialErrors } =
    validationRules
        |> List.foldr
            (\{ fieldName, fieldRules, fieldValue } sumErrors ->
                fieldRules
                    |> List.map
                        (\toValidation ->
                            ( fieldName, toValidation, fieldValue ) :: sumErrors
                        )
                    |> List.concat
            )
            []
        |> List.foldl
            (\( fieldName, toValidation, fieldValue ) sumErrors ->
                updateError fieldName sumErrors (toValidation fieldValue)
            )
            --  Scoping down to errors that are related to the validation rules
            (initialErrors
                |> Dict.filter
                    (\errorId _ ->
                        List.any
                            (\{ fieldName } ->
                                fieldName == errorId
                            )
                            validationRules
                    )
            )


updateError :
    String
    -> Dict String (List String)
    -> Validation
    -> Dict String (List String)
updateError field errors validation =
    let
        errorMsg : String
        errorMsg =
            validationToErrorMsg validation
    in
    if Dict.member field errors then
        Dict.update
            field
            (Maybe.map
                (modifyErrorPerField (shouldInsertError validation) errorMsg)
            )
            errors

    else if shouldInsertError validation then
        Dict.insert field [ errorMsg ] errors

    else
        errors


shouldInsertError : Validation -> Bool
shouldInsertError validation =
    case validation of
        CheckInvalidField field ->
            Utils.checkEmpty field

        InvalidChoosableField field ->
            Utils.checkEmpty field

        SpecialCharacter input ->
            Utils.checkAllSpecChar input

        CheckForIntInInput input ->
            String.any Char.isDigit input

        CheckEmptyEmail email ->
            Utils.checkEmpty email

        CheckInvalidEmail email ->
            not <| Utils.isValidEmail email

        CheckStringTooShort minimum str ->
            String.length str < minimum

        CheckStringTooLong maximum str ->
            String.length str > maximum

        CheckEmptyName name ->
            Utils.checkEmpty name

        CheckFileSize size value ->
            Utils.checkFileSize size value

        CheckFileType fileTypes value ->
            Utils.checkFileType fileTypes value

        CheckEmptyPassword password ->
            Utils.checkEmpty password

        CheckPasswordTooShort minimum password ->
            Utils.checkLength password minimum

        CheckPasswordCapitalize password ->
            not <| Utils.checkCapitalized password

        CheckForBusiness email ->
            not <| Utils.checkForBusinessEmail email

        CheckPasswordSpecialChar password ->
            not <| Utils.checkSpecChar password

        CheckPasswordContainsInt password ->
            not <| Utils.checkInt password

        CheckPasswordMatch password confirmPassword ->
            not <| Utils.checkMatch password confirmPassword

        CheckEmptyCvc cvc ->
            Utils.checkEmpty cvc

        CheckCvcLength cvc ->
            Utils.checkCvc cvc

        CheckEmptyExparation e ->
            Utils.checkEmpty e

        CheckExparation currentZone currentDate exparationDate ->
            not <| Utils.checkExpiration currentZone currentDate exparationDate

        CheckEmptyPhoneNumber pn ->
            Utils.checkEmpty pn

        CheckEmptyCard c ->
            Utils.checkEmpty c

        CheckCard c ->
            not <| Utils.checkCard c

        CheckPhoneNumber pn ->
            not <| Utils.checkPhone pn

        CheckEmptyCompany c ->
            Utils.checkEmpty c

        CheckName n ->
            not <| Utils.checkName n

        CheckShouldMatch lst s ->
            let
                s_ =
                    s
                        |> String.trim
                        |> String.toLower

                lst_ =
                    lst
                        |> List.map
                            (\str ->
                                str
                                    |> String.trim
                                    |> String.toLower
                            )
            in
            not <| List.member s_ lst_

        CheckForDuplicate lst s ->
            let
                s_ =
                    s
                        |> String.trim
                        |> String.toLower

                lst_ =
                    lst
                        |> List.map
                            (\str ->
                                str
                                    |> String.trim
                                    |> String.toLower
                            )
            in
            List.member s_ lst_

        CheckIfListHaveMinimumItems lst minItemCount _ ->
            List.length lst < minItemCount


modifyErrorPerField : Bool -> String -> List String -> List String
modifyErrorPerField checkError errorMsg errs =
    case ( checkError, List.member errorMsg errs ) of
        ( False, True ) ->
            List.filter (\err -> err /= errorMsg) errs

        ( True, False ) ->
            List.append errs [ errorMsg ]

        _ ->
            errs


anyActiveError : Dict String (List String) -> Bool
anyActiveError errors =
    errors
        |> Dict.values
        |> List.all List.isEmpty
        |> not


resetErrorsPerField : String -> Dict String (List String) -> Dict String (List String)
resetErrorsPerField key errors =
    Dict.filter (\k _ -> k /= key) errors

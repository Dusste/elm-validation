# Elm Validation

Form-oriented validation for Elm: you describe **fields**, **rules** (as `String -> Validation`), and **current values**; `checkErrors` returns a `Dict String (List String)` of messages keyed by field id. A small **Error** module helps render those messages next to inputs.

This repository is an Elm **application** that bundles the validation modules with a demo UI (`Main.elm`). To publish the validation code as an Elm package, you would typically move it into a `package` project in `elm.json`, expose only the modules you want (`Validation`, `Error`, and optionally a slimmer `Util`), and keep the demo in a separate app or example folder.

## Modules

### `Validation`

**Types**

- **`Config`** — `{ validationRules : List ValidationRule, initialErrors : Dict String (List String) }`. Pass your previous error dict as `initialErrors` so unrelated keys are preserved only for fields that appear in `validationRules` (see `checkErrors`).
- **`ValidationRule`** — `{ fieldName : String, fieldRules : List (String -> Validation), fieldValue : String }`. Each rule is a function that takes the field’s string value and returns a `Validation` variant.
- **`Validation`** — Sum type of checks. When a check _fails_, its human-readable message is added for that `fieldName` (see `shouldInsertError` / `validationToErrorMsg` in the source).

**Functions**

- **`checkErrors : Config -> Dict String (List String)`** — Runs all rules, merges results into the error dict.
- **`updateError : String -> Dict String (List String) -> Validation -> Dict String (List String)`** — Insert or remove one field’s message for a single `Validation` outcome (used internally by `checkErrors`; useful if you extend the pipeline).
- **`anyActiveError : Dict String (List String) -> Bool`** — `True` if any field has at least one error message.
- **`resetErrorsPerField : String -> Dict String (List String) -> Dict String (List String)`** — Clears all errors for one field key (typical on `onInput`).

**`Validation` variants (summary)**

| Area                               | Variants                                                                                                                                               |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Generic                            | `CheckInvalidField`, `InvalidChoosableField`, `SpecialCharacter`, `CheckForIntInInput`                                                                 |
| Email                              | `CheckEmptyEmail`, `CheckInvalidEmail`, `CheckForBusiness`                                                                                             |
| Password                           | `CheckEmptyPassword`, `CheckPasswordTooShort`, `CheckPasswordCapitalize`, `CheckPasswordSpecialChar`, `CheckPasswordContainsInt`, `CheckPasswordMatch` |
| Length                             | `CheckStringTooShort`, `CheckStringTooLong`                                                                                                            |
| Name / company                     | `CheckEmptyName`, `CheckName`, `CheckEmptyCompany`                                                                                                     |
| Card / CVC / expiry                | `CheckEmptyCard`, `CheckCard`, `CheckEmptyCvc`, `CheckCvcLength`, `CheckEmptyExparation`, `CheckExparation` (needs `Time.Zone` and `Time.Posix`)       |
| Phone                              | `CheckEmptyPhoneNumber`, `CheckPhoneNumber`                                                                                                            |
| Files (string content / data URLs) | `CheckFileSize`, `CheckFileType`                                                                                                                       |
| Lists / matching                   | `CheckShouldMatch`, `CheckForDuplicate`, `CheckIfListHaveMinimumItems`                                                                                 |

Messages are fixed English strings in `validationToErrorMsg` (see `src/Validation.elm`).

### `Error`

Helpers for displaying `Dict String (List String)` in `elm/html` views (styled for Tailwind-style classes already used in the demo).

- **`byFieldName : String -> Dict String (List String) -> List String`**
- **`hasError : String -> Dict String (List String) -> Bool`**
- **`viewError : List String -> Html msg`** — Renders a red alert block.
- **`withStandaloneField : List String -> Html msg`** — Same idea as `viewError` but with spacing (`mt-1`); handy when the field is not a plain `input`.

### `Util`

Low-level checks and file/string utilities used by `Validation` (module currently exposes `(..)`). Includes email heuristics, business-vs-personal email domain list, card/CVC/phone/name checks, regex-based special-character detection, **data URL** parsing, **base64** and **binary** size estimation for `File.toString` / URL content, and **magic-byte** style file type detection for common PDF, Office ZIP, and image formats.

If you publish a package, consider exposing only what consumers need or splitting helpers into focused modules.

## Usage sketch

```elm
import Dict
import Validation

type alias Model =
    { email : String
    , formErrors : Dict String (List String)
    }

onSubmit : Model -> Model
onSubmit model =
    let
        errors =
            Validation.checkErrors
                { validationRules =
                    [ { fieldName = "email"
                      , fieldRules =
                            [ Validation.CheckEmptyEmail
                            , Validation.CheckInvalidEmail
                            ]
                      , fieldValue = model.email
                      }
                    ]
                , initialErrors = model.formErrors
                }
    in
    if Validation.anyActiveError errors then
        { model | formErrors = errors }

    else
        model
```

On input, clear that field’s errors with `Validation.resetErrorsPerField "email" model.formErrors`.

## 🚀 Demo

## Explore the live demo: [https://dusste.github.io/elm-validation](https://dusste.github.io/elm-validation)

## Running the demo

```bash
npm install
# build steps as defined in package.json, e.g.:
npx elm make src/Main.elm --output=dist/elm.js
```

Then open `index.html` (or use your usual static server). The demo covers textarea min/max length, file size/type, email, a required select, and matching a typed value against a list (e.g. user names).

## Dependencies

- **Elm 0.19.1** — see `elm.json`.
- **Direct Elm libs**: `elm/core`, `elm/html`, `elm/time`, `elm/regex`, plus `elm/file` / `elm/http` / `elm/browser` for the demo app.
- **elm-community/string-extra** — used in `Util` for email helpers.

## Notes

- Email validation is intentionally simple; see comments in `Util.isValidEmail`.
- Some user-facing strings in `Validation` may still be tweaked (there is an in-source TODO on error messaging).
- Expiry validation needs the current zone and time passed into `CheckExparation`.

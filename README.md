# Elm Validation

Form-oriented validation for Elm: you describe **fields**, **rules** (as `String -> Validation`), and **current values**; `checkErrors` returns a `Dict String (List String)` of messages keyed by field id. A small **Error** module helps render those messages next to inputs.

This repository is an Elm **application** that bundles the validation modules with a demo UI (`Main.elm`). To publish the validation code as an Elm package, you would typically move it into a `package` project in `elm.json`, expose only the modules you want (`Validation`, `Error`, and optionally a slimmer `Util`), and keep the demo in a separate app or example folder.

## Modules

### `Validation`

**Types**

- **`Config`** — `{ validationRules : List ValidationRule, initialErrors : Dict String (List String) }`. Pass your previous error dict as `initialErrors` so unrelated keys are preserved only for fields that appear in `validationRules` (see `checkErrors`).
- **`ValidationRule`** — `{ fieldName : String, fieldRules : List (String -> Validation), fieldValue : String }`. Each rule is a function that takes the field’s string value and returns a `Validation` variant. Use closures when a rule needs extra data (e.g. `CheckPasswordMatch`, `CheckExparation`, or `CheckIfListHaveMinimumItems` with a list from your model).
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

## Demo

Live build: [https://dusste.github.io/elm-validation](https://dusste.github.io/elm-validation)

The interactive app in `src/Main.elm` exercises almost every `Validation` variant on submit:

- **Length** — textarea min/max character count
- **Files** — max size and allowed types (content read as data URL / string)
- **Email** — empty + format on one field; separate **business email** field (rejects common personal domains)
- **Password** — empty, minimum length, capital letter, digit, special character, and **match** with confirmation
- **Name** — empty + two alphabetic words (`CheckName`)
- **Phone** — empty + pattern
- **Card / CVC / expiry** — Luhn card, 3-digit CVC, `MM/YY` not expired (`CheckExparation` uses `Time.here` / `Time.now` on init and a periodic clock tick)
- **Letters-only field** — `CheckForIntInInput` and `SpecialCharacter` together (no digits, no punctuation)
- **Slug** — `CheckInvalidField` plus `CheckForDuplicate` against reserved names
- **Skills checkboxes** — `CheckIfListHaveMinimumItems` on a list held in the model (the rule’s string argument is unused)
- **Company** — `CheckEmptyCompany`
- **Note** — generic required text via `CheckInvalidField`
- **Select** — `InvalidChoosableField`
- **User picker** — `CheckShouldMatch` against a list of allowed names

## Running locally

```bash
npm install
npm start
```

This runs Elm compile to `dist/elm.js` and Tailwind in watch mode (`tailwind.css` → `dist/tailwind.build.css`). Open `index.html` in the browser (or serve the project root with any static file server).

For a one-off Elm build without watch:

```bash
npx elm make src/Main.elm --output=dist/elm.js
```

## Dependencies

- **Elm 0.19.1** — see `elm.json`.
- **Direct Elm libs**: `elm/core`, `elm/html`, `elm/time`, `elm/regex`, plus `elm/file` / `elm/http` / `elm/browser` for the demo app.
- **elm-community/string-extra** — used in `Util` for email helpers.
- **Node** (optional tooling) — `elm`, `tailwindcss`, `concurrently` via `package.json` for local dev and CSS.

## Notes

- Email validation is intentionally simple; see comments in `Util.isValidEmail`.
- Some user-facing strings in `Validation` may still be tweaked (there is an in-source TODO on error messaging).
- `CheckExparation` needs the current zone and time; until both are available, the demo only enforces non-empty expiry (see `Main.elm` submit config).

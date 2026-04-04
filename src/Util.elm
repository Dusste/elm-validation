module Util exposing (..)

import Regex
import String.Extra
import Time


bytesToKilobytes : Int -> String
bytesToKilobytes bytes =
    let
        -- Use decimal conversion (1000 bytes = 1 KB) for user-friendly display
        -- This matches common expectations: 200000 bytes = 200 KB
        kilobytes =
            round (toFloat bytes / 1000)
    in
    String.fromInt kilobytes ++ " KB"


{-| Convert MIME type to file extension
-}
mimeTypeToFileExtension : String -> String
mimeTypeToFileExtension mimeType =
    case String.split "/" mimeType of
        [ _, subType ] ->
            let
                normalizedSubType =
                    String.toLower subType
            in
            -- Handle common MIME types
            if normalizedSubType == "vnd.openxmlformats-officedocument.wordprocessingml.document" || normalizedSubType == "msword" then
                "docx"

            else if normalizedSubType == "vnd.openxmlformats-officedocument.spreadsheetml.sheet" || normalizedSubType == "vnd.ms-excel" then
                "excel"

            else if normalizedSubType == "jpg" then
                "jpeg"

            else
                normalizedSubType

        _ ->
            String.toLower mimeType


getCurrentYearMonth : Time.Zone -> Time.Posix -> ( Int, Int )
getCurrentYearMonth zone time =
    let
        year =
            Time.toYear zone time

        month =
            Time.toMonth zone time
    in
    ( year, monthToInt month )


monthToInt : Time.Month -> Int
monthToInt m =
    case m of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


{-| Detect file type from data URL MIME type
-}
detectFileTypeFromDataUrl : String -> Maybe String
detectFileTypeFromDataUrl dataUrl =
    extractDataUrlParts dataUrl
        |> Maybe.map (Tuple.first >> mimeTypeToFileExtension)


{-| -- Calculate actual file size in bytes from content string
-- Handles both base64-encoded content and raw binary content
-- Also handles data URLs (<data:mime/type;base64,..>.)
-}
calculateFileSizeInBytes : String -> Int
calculateFileSizeInBytes content =
    -- Extract actual content from data URL if present
    let
        actualContent =
            case extractDataUrlParts content of
                Just ( _, base64Content ) ->
                    base64Content

                Nothing ->
                    content
    in
    if String.isEmpty actualContent then
        0

    else if isBase64Encoded actualContent then
        -- Base64 encoding: 4 characters represent 3 bytes
        -- Padding (=) indicates fewer bytes in the last group
        let
            -- Count padding characters at the end (= or ==)
            -- Base64 padding is always at the end: 0, 1, or 2 '=' characters
            paddingCount =
                if String.endsWith "==" actualContent then
                    2

                else if String.endsWith "=" actualContent then
                    1

                else
                    0

            baseLength =
                String.length actualContent - paddingCount

            -- Calculate: (baseLength * 3) // 4
            -- Padding doesn't add bytes, it just indicates the last group has fewer bytes
            -- Formula: ((length - padding) * 3) // 4
            actualSize =
                (baseLength * 3) // 4
        in
        actualSize

    else
        -- For binary files (PDF, images, etc.) read as UTF-8 strings:
        -- When File.toString reads binary files, the string representation
        -- may not accurately reflect the original byte count due to UTF-8 encoding.
        --
        -- Check if it's a known binary format or has binary characteristics
        let
            isBinaryFile =
                String.startsWith "%PDF-" actualContent
                    || String.startsWith "PK" actualContent
                    || String.startsWith "GIF8" actualContent
                    || (String.length actualContent
                            > 0
                            && (let
                                    firstCharCode =
                                        actualContent
                                            |> String.uncons
                                            |> Maybe.map (Tuple.first >> Char.toCode)
                                in
                                firstCharCode
                                    == Just 255
                                    || firstCharCode
                                    == Just 137
                               )
                       )
                    || hasHighBinaryContent actualContent

            -- Check if content has a high percentage of non-printable/binary characters
            -- This helps catch binary files that don't match known signatures
            hasHighBinaryContent : String -> Bool
            hasHighBinaryContent str =
                let
                    chars =
                        String.toList str

                    totalChars =
                        List.length chars

                    -- Count characters that are likely from binary data:
                    -- Control chars (0-31 except common ones), high bytes (128-255)
                    binaryCharCount =
                        chars
                            |> List.filter
                                (\c ->
                                    let
                                        code =
                                            Char.toCode c
                                    in
                                    -- High bytes (128-255) are common in binary files
                                    code
                                        >= 128
                                        -- Or control characters (except common ones like \n, \r, \t)
                                        || (code < 32 && code /= 10 && code /= 13 && code /= 9)
                                )
                            |> List.length

                    -- If more than 10% of characters are binary-like, treat as binary
                    binaryRatio =
                        toFloat binaryCharCount / toFloat (max 1 totalChars)
                in
                binaryRatio > 0.1 && totalChars > 100
        in
        if isBinaryFile then
            -- Binary file: When File.toString reads binary files as UTF-8 strings,
            -- the relationship between string length and actual byte size varies.
            -- For some files, string length overestimates; for others, it's close.
            -- Use String.length directly as it's often a reasonable approximation
            -- for binary files (each byte often maps to one character, even if
            -- some become replacement characters).
            String.length actualContent

        else
            -- For other content, if it's large (>10KB string length),
            -- it's likely binary data that wasn't caught by the binary detection.
            -- Apply the same correction factor to be safe.
            let
                stringLength =
                    String.length actualContent
            in
            if stringLength > 10000 then
                -- Large content: might be binary, use string length as approximation
                stringLength

            else
                -- Small content: treat as text and count UTF-8 bytes accurately
                estimateByteSizeFromUtf8String actualContent


{-| -- Check if content appears to be base64 encoded
-}
isBase64Encoded : String -> Bool
isBase64Encoded str =
    -- Handle data URLs: extract base64 content first
    let
        contentToCheck =
            case extractDataUrlParts str of
                Just ( _, base64Content ) ->
                    base64Content

                Nothing ->
                    str
    in
    -- First check: if it starts with known binary signatures, it's NOT base64
    if
        String.startsWith "%PDF-" contentToCheck
            || String.startsWith "PK" contentToCheck
            || String.startsWith "GIF8" contentToCheck
    then
        False

    else
        let
            -- Base64 characters: A-Z, a-z, 0-9, +, /, and = for padding
            isBase64Char : Char -> Bool
            isBase64Char char =
                Char.isAlphaNum char
                    || char
                    == '+'
                    || char
                    == '/'
                    || char
                    == '='

            -- Base64 strings are typically longer and have length multiple of 4 (after trimming)
            -- and contain only base64 characters
            allBase64Chars =
                contentToCheck
                    |> String.toList
                    |> List.all isBase64Char

            -- Check if length is reasonable for base64 (multiple of 4, or close to it)
            reasonableLength =
                let
                    len =
                        String.length contentToCheck
                in
                len > 4 && (modBy 4 len == 0 || modBy 4 (len - 1) == 0 || modBy 4 (len - 2) == 0)
        in
        allBase64Chars && reasonableLength


{-| Check if string is a data URL (<data:mime/type;base64,..>.)
-}
isDataUrl : String -> Bool
isDataUrl str =
    String.startsWith "data:" str


specialCharacterRegex : Regex.Regex
specialCharacterRegex =
    Regex.fromString "[!-\\/:-@[-`{-~]"
        |> Maybe.withDefault Regex.never


allSpecialCharactersRegex : Regex.Regex
allSpecialCharactersRegex =
    Regex.fromString "[!\"#$%&'()*+,\\-./:;<=>?@\\[\\\\\\]^_`{|}~]"
        --it should allow normal characters like a-z, A-Z, 0-9, and spaces
        |> Maybe.withDefault Regex.never


{-| -- Estimate byte size from UTF-8 string
-- For binary content, most characters are single-byte, but we need to account for multi-byte UTF-8
-}
estimateByteSizeFromUtf8String : String -> Int
estimateByteSizeFromUtf8String str =
    -- Count UTF-8 bytes by examining character codes
    -- Characters with code <= 127 are 1 byte
    -- Characters with code 128-2047 are 2 bytes
    -- Characters with code 2048-65535 are 3 bytes
    -- Characters with code > 65535 are 4 bytes
    str
        |> String.toList
        |> List.map Char.toCode
        |> List.map
            (\code ->
                if code <= 127 then
                    1

                else if code <= 2047 then
                    2

                else if code <= 65535 then
                    3

                else
                    4
            )
        |> List.sum


personalEmailDomains : List String
personalEmailDomains =
    [ "gmail.com"
    , "yahoo.com"
    , "yahoo.co.uk"
    , "hotmail.com"
    , "outlook.com"
    , "live.com"
    , "aol.com"
    , "icloud.com"
    , "me.com"
    , "msn.com"
    , "mail.com"
    , "zoho.com"
    , "protonmail.com"
    , "gmx.com"
    , "yandex.com"
    , "inbox.ru"
    , "mail.ru"
    , "bk.ru"
    , "list.ru"
    , "tutanota.com"
    , "hushmail.com"
    , "fastmail.com"
    , "runbox.com"
    , "qq.com"
    , "naver.com"
    , "daum.net"
    , "seznam.cz"
    , "web.de"
    , "freenet.de"
    , "rediffmail.com"
    , "163.com"
    , "126.com"
    , "yeah.net"
    ]


checkForBusinessEmail : String -> Bool
checkForBusinessEmail str =
    personalEmailDomains
        |> List.filter (\d -> String.contains d str)
        |> List.length
        |> (==) 0


checkEmpty : String -> Bool
checkEmpty str =
    let
        trim : String
        trim =
            String.trim str
    in
    String.length trim == 0


checkAllSpecChar : String -> Bool
checkAllSpecChar str =
    let
        trim : String
        trim =
            String.trim str
    in
    Regex.contains allSpecialCharactersRegex trim


{-| Extract MIME type and base64 content from data URL
Returns: Just (mimeType, base64Content) or Nothing
-}
extractDataUrlParts : String -> Maybe ( String, String )
extractDataUrlParts str =
    if not (isDataUrl str) then
        Nothing

    else
        -- Format: data:mime/type;base64,content
        case String.split "," str of
            [ prefix, content ] ->
                -- Extract MIME type from prefix (data:mime/type;base64)
                if String.endsWith ";base64" prefix then
                    let
                        mimeType =
                            prefix
                                |> String.dropLeft 5
                                |> String.dropRight 7
                    in
                    Just ( mimeType, content )

                else
                    Nothing

            _ ->
                Nothing


{-| (Still) Incomplete email check
-}
isValidEmail : String -> Bool
isValidEmail input =
    case String.split "@" input of
        [ beforeEt, afterEtWithDot ] ->
            let
                beforeEtIsValid : Bool
                beforeEtIsValid =
                    (String.length beforeEt > 0) && not (String.contains " " beforeEt)

                afterEtWithDotIsValid : Bool
                afterEtWithDotIsValid =
                    case String.split "." afterEtWithDot of
                        [ afterEt, afterDot ] ->
                            let
                                validate v =
                                    not (String.Extra.isBlank v) && not (String.contains " " v)
                            in
                            validate afterEt && validate afterDot

                        _ ->
                            False
            in
            beforeEtIsValid && afterEtWithDotIsValid

        _ ->
            False


checkFileSize : Int -> String -> Bool
checkFileSize size value =
    let
        actualSizeInBytes : Int
        actualSizeInBytes =
            calculateFileSizeInBytes value
    in
    actualSizeInBytes >= size



{-
   return True if valid
-}


checkName : String -> Bool
checkName input =
    let
        words : List String
        words =
            input
                |> String.trim
                |> String.words

        validWords : List String
        validWords =
            List.filter (\word -> String.all Char.isAlpha word) words
    in
    List.length validWords >= 2


checkFileType : List String -> String -> Bool
checkFileType fileTypes value =
    let
        -- Normalize file type (remove dot if present, convert to lowercase, handle aliases)
        normalizeFileType : String -> String
        normalizeFileType fileType =
            fileType
                |> String.trim
                |> (\s ->
                        if String.startsWith "." s then
                            String.dropLeft 1 s

                        else
                            s
                   )
                |> String.toLower
                |> (\s ->
                        -- Normalize "jpg" to "jpeg" for consistency
                        if s == "jpg" then
                            "jpeg"

                        else
                            s
                   )

        -- Detect file type based on content signatures (magic bytes)
        detectFileTypeFromContent : String -> Maybe String
        detectFileTypeFromContent content =
            if String.isEmpty content then
                Nothing

            else if String.startsWith "%PDF-" content then
                Just "pdf"

            else if String.startsWith "PK" content then
                -- ZIP-based formats (DOCX, XLSX, etc.)
                -- Check for Office Open XML signatures in the content
                if String.contains "word/" content || String.contains "[Content_Types].xml" content then
                    Just "docx"

                else if String.contains "xl/" content || String.contains "worksheets/" content then
                    Just "excel"

                else
                    -- Generic ZIP, assume DOCX as fallback
                    Just "docx"

            else
                -- Check for image formats
                -- When binary files are read as UTF-8 strings, byte values might not be preserved
                -- Use more robust detection methods
                let
                    firstCharCode : Maybe Int
                    firstCharCode =
                        content
                            |> String.uncons
                            |> Maybe.map (Tuple.first >> Char.toCode)

                    secondCharCode : Maybe Int
                    secondCharCode =
                        content
                            |> String.dropLeft 1
                            |> String.uncons
                            |> Maybe.map (Tuple.first >> Char.toCode)

                    -- PNG signature: 89 50 4E 47 (bytes 137, 80, 78, 71)
                    -- When read as UTF-8, byte 0x89 might become a replacement char (65533)
                    -- or be part of a multi-byte sequence. The most reliable check is to look
                    -- for "PNG" starting at position 1 (after the first byte/char).
                    isPNG =
                        String.length content
                            > 4
                            && String.startsWith "PNG" (String.dropLeft 1 content)
                            && (firstCharCode
                                    == Just 137
                                    || firstCharCode
                                    == Just 65533
                                    -- Also accept if first char is high byte (128-255) indicating binary
                                    || (Maybe.map (\code -> code >= 128 && code <= 255) firstCharCode
                                            |> Maybe.withDefault False
                                       )
                               )

                    -- JPEG signature: FF D8 (bytes 255, 216) - the third byte can vary
                    -- When read as UTF-8:
                    -- - Byte 0xFF (255) is invalid UTF-8, becomes replacement char (65533)
                    -- - Byte 0xD8 (216) is a continuation byte, might become replacement char (65533) or be preserved
                    isJPEG =
                        -- Check exact pattern if bytes are preserved
                        (firstCharCode == Just 255 && secondCharCode == Just 216)
                            -- Check if first is replacement char and second is 216 (most common case)
                            || (firstCharCode == Just 65533 && secondCharCode == Just 216)
                            -- Check if both are replacement chars (both bytes invalid UTF-8)
                            || (firstCharCode == Just 65533 && secondCharCode == Just 65533)
                            -- Check if first is high byte (240-255) and second is 216
                            || (secondCharCode
                                    == Just 216
                                    && (Maybe.map (\code -> code >= 240 && code <= 255) firstCharCode
                                            |> Maybe.withDefault False
                                       )
                               )
                            -- Check if first is replacement and second is 216 or also replacement
                            || (firstCharCode
                                    == Just 65533
                                    && (secondCharCode == Just 216 || secondCharCode == Just 65533)
                               )
                in
                if isPNG then
                    Just "png"

                else if isJPEG then
                    Just "jpeg"

                else if String.startsWith "GIF8" content then
                    Just "gif"

                else
                    Nothing

        normalizedFileTypes : List String
        normalizedFileTypes =
            List.map normalizeFileType fileTypes

        -- Try to detect file type: first from data URL MIME type, then from content
        detectedType : Maybe String
        detectedType =
            if isDataUrl value then
                -- For data URLs, use MIME type detection
                -- Note: Content-based detection would require base64 decoding,
                -- so we rely on MIME type which is more reliable for data URLs
                detectFileTypeFromDataUrl value

            else
                -- For non-data URLs, use content-based detection
                detectFileTypeFromContent value
    in
    case detectedType of
        Just fileType ->
            List.member fileType normalizedFileTypes |> not

        Nothing ->
            True


checkLength : String -> Int -> Bool
checkLength str min =
    let
        trim : String
        trim =
            String.trim str
    in
    String.length trim < min


checkCapitalized : String -> Bool
checkCapitalized str =
    let
        trim : String
        trim =
            String.trim str
    in
    String.any Char.isUpper trim


checkSpecChar : String -> Bool
checkSpecChar str =
    let
        trim : String
        trim =
            String.trim str
    in
    Regex.contains specialCharacterRegex trim


checkInt : String -> Bool
checkInt str =
    let
        trim : String
        trim =
            String.trim str
    in
    Regex.contains
        (Regex.fromString "\\d"
            |> Maybe.withDefault Regex.never
        )
        trim


checkMatch : String -> String -> Bool
checkMatch str1 str2 =
    let
        trim1 : String
        trim1 =
            String.trim str1

        trim2 : String
        trim2 =
            String.trim str2
    in
    trim1 == trim2


checkCvc : String -> Bool
checkCvc str =
    let
        trim : String
        trim =
            String.trim str
    in
    String.length trim /= 3


validExpirationFormat : Maybe Regex.Regex
validExpirationFormat =
    Regex.fromString "^(0[1-9]|1[0-2])/([0-9]{2})$"


checkExpiration : Time.Zone -> Time.Posix -> String -> Bool
checkExpiration zone currentDate expirationDate =
    let
        ( currentYear, currentMonth ) =
            getCurrentYearMonth zone currentDate
    in
    case validExpirationFormat of
        Just validFormat ->
            if Regex.contains validFormat expirationDate then
                case String.split "/" expirationDate of
                    [ mmStr, yyStr ] ->
                        case ( String.toInt mmStr, String.toInt yyStr ) of
                            ( Just mm, Just yy ) ->
                                let
                                    shortYear =
                                        modBy 100 currentYear
                                in
                                yy > shortYear || (yy == shortYear && mm >= currentMonth)

                            _ ->
                                False

                    _ ->
                        False

            else
                False

        Nothing ->
            False


checkCard : String -> Bool
checkCard str =
    let
        cleaned =
            cleanCardNumber str

        len =
            String.length cleaned
    in
    (len >= 13 && len <= 19) && luhnCheck str


cleanCardNumber : String -> String
cleanCardNumber str =
    String.filter Char.isDigit str


checkPhone : String -> Bool
checkPhone input =
    case maybeValidPhone of
        Just regex ->
            Regex.contains regex input

        Nothing ->
            False


maybeValidPhone : Maybe Regex.Regex
maybeValidPhone =
    Regex.fromString "^[+]?([0-9][ -]?){6,15}[0-9]$"


luhnCheck : String -> Bool
luhnCheck rawInput =
    let
        digitsReversed =
            cleanCardNumber rawInput
                |> String.reverse
                |> String.toList
                |> List.filter Char.isDigit
                |> List.map Char.toCode
                |> List.map (\c -> c - Char.toCode '0')

        sumDigits index digit =
            if modBy 2 index == 1 then
                let
                    doubled =
                        digit * 2
                in
                if doubled > 9 then
                    doubled - 9

                else
                    doubled

            else
                digit

        total =
            digitsReversed
                |> List.indexedMap sumDigits
                |> List.sum
    in
    (total |> modBy 10) == 0

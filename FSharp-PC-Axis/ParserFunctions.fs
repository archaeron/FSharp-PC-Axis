module FSharp.Data.PCAxis.ParserFunctions

open FSharp.Data.PCAxis.ParserTypes
open FParsec

let private charListToString cs = System.String(Array.ofList cs)
let private betweenChars c1 c2 = between (pchar c1) (pchar c2)
let private betweenOptionalChars c1 c2 = between (optional (pchar c1)) (optional (pchar c2))

/// The PC-Axis format saves it's boolean values as YES and NO
let private yesNoToBool =
    function
    | "YES" -> true
    | _ -> false

let private whiteSpace = spaces
/// parse zero or more spaces
let private spaces = skipMany << pchar <| ' '

/// An identifier is made of one or more uppercase ascii charactes, digits or '-'.
let private uppercaseIdentifierParser = (many1 (asciiUpper <|> digit <|> pchar '-') <?> "uppercase identifier") |>> charListToString

let private identifierParser = many1 (noneOf "()[]'\"") |>> charListToString

let private integerParser = pint32 |>> Integer

/// a subkey is an identifier between parentheses and optionally between quotes
let private subKeyParser = betweenChars '(' ')' (betweenOptionalChars '"' '"' identifierParser)

/// the language is set between brackets
let private languageParser = betweenChars '[' ']' identifierParser

/// a keyword consists of an uppercase identifier and optionally of a language and/or a subkey
let private keywordParser = (tuple3 uppercaseIdentifierParser (opt languageParser) (opt subKeyParser)) |>> Keyword

/// booleans consist of "YES" (true) and "NO" (false)
let private booleanParser = pstring "YES" <|> pstring "NO" |>> (yesNoToBool >> Bool)

/// a text must be between quotes and not consist of quotes
let private textParser = betweenChars '"' '"' (many1 (noneOf "\"")) |>> (charListToString >> TextData)

let private listParser = sepBy1 textParser (pchar ',' .>> (optional newline)) |>> List

let private valueParser = integerParser <|> booleanParser <|> textParser <|> listParser

let private dataValuesSeparator = (skipMany1 (anyOf ",; \t")) <?> "data values separator"
let private dataValuesLineSeparator = newline
let private dataValueParser = (integerParser <|> textParser) <?> "a data value"
let private dataValuesLineParser = (sepBy dataValueParser dataValuesSeparator)
let private dataValuesParser = sepBy dataValuesLineParser dataValuesLineSeparator
let private dataParser = (pstring "DATA=" >>. newline) >>. dataValuesParser .>> eof

let private assignmentSignParser = pchar '=' .>> (optional newline)

let private dataLine = (dataParser |>> (fun v -> DataEntry (Keyword ("DATA", None, None), Values v)))
let private keywordLine = spaces >>. (tuple3 keywordParser assignmentSignParser valueParser) .>> spaces |>> (fun (key, _, value) -> DataEntry (key, value))

let private lineParser = dataLine <|> keywordLine

let private parseLine = run lineParser

let private trimLine (line: string) =
    let trimmedLine = line.Trim()
    let trimRegex = new System.Text.RegularExpressions.Regex(" +(\n|\r\n)")
    trimRegex.Replace(trimmedLine, "\n")

let private read (lines: string seq) =
    lines
        |> Seq.filter (fun l -> l <> "")
        |> Seq.map trimLine
        |> Seq.map parseLine

let parse (text: string) =
    let splitLines = text.Split(';')
    read splitLines

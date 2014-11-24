module FSharp.Data.PCAxis.ParserTypes

type Language = string
type SubKey = string
type Keyword = Keyword of string * Language option * SubKey option
type Data =
    | TextData of string
    | Integer of int
    | Bool of bool
    | List of Data list
    | Values of Data list list

type DataEntry = DataEntry of Keyword * Data
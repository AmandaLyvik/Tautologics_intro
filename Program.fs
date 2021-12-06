
open FParsec
open System.IO
open System.Text

Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)

// -------------------------------------------------------------

let ws = many1Satisfy (function ' '|'\t' -> true | _ -> false)

// Parse a lable
let identifier : Parser<string, unit> =
    let isIdentifierFirstChar c = c ='#'
    let isIdentifierChar c = isUpper c

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws// the "identifier" is used in error messages

let word = many1Satisfy (function ' '|'\t'|'"' -> false | _ -> true)
let str = stringsSepBy word ws

let value = 
    choice [pstring "\"\"" 
            between (pstring "\"") (pstring "\"") str 
            word ] .>> ws

let item = identifier .>>. many1 value .>> newline

// run 'item' until eof
let SIE = many1 item .>> eof

// -------------------------------------------------------------

// Get the file path
let baseDirectory = __SOURCE_DIRECTORY__
let fileName = "test.SE"
let filePath = Path.Combine(baseDirectory, "sample_SIE", fileName)

// Parse from file
let enc = Encoding.GetEncoding(437)
let result = runParserOnFile SIE () filePath enc
             
System.Console.WriteLine(sprintf "%A" result)
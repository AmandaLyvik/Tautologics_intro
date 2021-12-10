open FParsec
open System.IO
open System.Text

Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
 
// -------------------------------------------------------------

let ws = many1SatisfyL (function ' '|'\t' -> true | _ -> false) "whiteSpece"

let id = 
    let firstChar c = c = '#'
    let remainingChar c = isUpper c 

    many1Satisfy2L firstChar remainingChar "identifier" .>> ws

let word : Parser<string,unit> =
    many1SatisfyL (function ' '|'"'|'\n' -> false | _ -> true) "word"

let str =
    between (pchar '"') (pchar '"') (stringsSepBy word ws)

let value = 
    choiceL [str;
             word] "value" .>> optional ws

let item =
    id .>>. many1 value .>> newline


let SIE =
    many1 item .>> eof

// -------------------------------------------------------------

// Get the file path
let baseDirectory = __SOURCE_DIRECTORY__
let fileName = "test.SE"
let filePath = Path.Combine(baseDirectory, "sample_SIE", fileName)

// Parse from file
let enc = Encoding.GetEncoding(437)
let result = runParserOnFile SIE () filePath enc
             
System.Console.WriteLine(sprintf "%A" result)
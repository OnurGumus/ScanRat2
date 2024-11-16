module Tests

open Fable.Core
open Fable.Core.JsInterop
open ScanRat.Tests.Grammars
open ScanRat.ScanRat
open ScanRat.Combinators
open System.Text.RegularExpressions

let inline equal (expected: 'T) (actual: 'T) : unit =
    Testing.Assert.AreEqual(expected, actual)


[<Global>]
let describe (name: string) (f: unit -> unit) = jsNative

[<Global>]
let it (msg: string) (f: unit -> unit) = jsNative

let rec compute exp =
    match exp with
    | Add(a, b) -> (compute a) + (compute b)
    | Subtract(a, b) -> (compute a) - (compute b)
    | Multiply(a, b) -> (compute a) * (compute b)
    | Divide(a, b) -> (compute a) / (compute b)
    | Number a -> a

let computeFromResult result =
    match result with
    | Success s -> compute s.Value
    | Failure _f -> failwith "can't compute, parsing failed"

let simpleCalc input =

    let exp = production "exp"

    exp.rule <-
        exp .+ ~~ "+" + exp --> Add
        |- exp .+ ~~ "-" + exp --> Subtract
        |- digits --> Number
        |- ~~ "(" +. exp .+ ~~ ")"

    parse exp input |> computeFromResult

let regexFailTest () =
    let digitParser = pRegex @"\d+" RegexOptions.IgnoreCase

    let wordParser = pRegex @"[A-Za-z]+" RegexOptions.IgnoreCase

    let wordThenDigitParser =
        wordParser + digitParser --> fun (words, digits) -> (words, digits)

    let wordThenDigitParserFull = ensureFullInputConsumed wordThenDigitParser
    let result = parse wordThenDigitParserFull "abc123 x"
    // Check the result
    match result with
    | Success s -> failwith "should fail"
    | Failure f -> (f.Index)

let regexTest () =
    let digitParser = pRegex @"\d+" RegexOptions.None

    let wordParser = pRegex @"[A-Za-z]+"  RegexOptions.IgnoreCase

    let wordThenDigitParser =
        wordParser + digitParser --> fun (words, digits) -> (words, digits)

    let wordThenDigitParserFull = ensureFullInputConsumed wordThenDigitParser
    let result = parse wordThenDigitParserFull "abc123"
    // Check the result
    match result with
    | Success s -> s.Value // |> should equal ("abc", "123")
    | Failure f -> failwithf "%A" (f.Index)

let precedenceCalc input =
    parse precedenceCalcExpression input |> computeFromResult



describe "my tests"
<| fun _ ->
    it "simple calc" <| fun () -> precedenceCalc "3-2/2*5+3*8+1" |> equal 23

    it "regex test" <| fun () -> regexTest () |> equal ("abc", "123")

    it "regex fail test" <| fun () -> regexFailTest () |> equal 6

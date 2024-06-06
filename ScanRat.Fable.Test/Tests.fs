module Tests

open Fable.Core
open Fable.Core.JsInterop
open ScanRat.Tests.Grammars
open ScanRat.ScanRat
let inline equal (expected: 'T) (actual: 'T): unit =
    Testing.Assert.AreEqual(expected, actual)


let [<Global>] describe (name: string) (f: unit->unit) = jsNative
let [<Global>] it (msg: string) (f: unit->unit) = jsNative

let rec compute exp =
        match exp with
        | Add (a, b) -> (compute a) + (compute b)
        | Subtract (a, b) -> (compute a) - (compute b)
        | Multiply (a, b) -> (compute a) * (compute b)
        | Divide (a, b) -> (compute a) / (compute b)
        | Number a -> a

let computeFromResult result =
    match result with
    | Success s -> compute s.Value
    | Failure _f -> failwith "can't compute, parsing failed"

let simpleCalc input =

    let exp = production "exp"
    exp.rule
        <- exp .+ ~~"+" + exp --> Add
        |- exp .+ ~~"-" + exp --> Subtract
        |- digits --> Number
        |- ~~"(" +. exp .+ ~~")"

    parse exp input |> computeFromResult

// from the IronMeta Project

let precedenceCalc input =
    parse precedenceCalcExpression input |> computeFromResult



describe "my tests" <| fun _ ->
    it "simple calc" <| fun () ->
     precedenceCalc "3-2/2*5+3*8+1" |> equal 23

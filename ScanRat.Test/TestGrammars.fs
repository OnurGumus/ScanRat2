module ScanRat.Tests.Grammars

open ScanRat.ScanRat

let digit = oneOf "0123456789" --> fun d -> int(d) - int('0')

let digits =
    let digits = production "digits"
    digits.rule
        <- digits + digit --> fun (a, b) -> a*10+b
        |- digit

    digits

type Exp =
    | Add of Exp * Exp
    | Subtract of Exp * Exp
    | Multiply of Exp * Exp
    | Divide of Exp * Exp
    | Number of int

let precedenceCalcExpression =
    let multiplicative = production "multiplicative"
    let additive = production "additive"

    let number = digits --> Number

    let add = additive .+ ~~"+" + multiplicative --> Add
    let sub = additive .+ ~~"-" + multiplicative --> Subtract

    let multiply = multiplicative .+ ~~"*" + number --> Multiply
    let divide = multiplicative .+ ~~"/" + number --> Divide

    additive.rule
        <- add
        |- sub
        |- multiplicative

    multiplicative.rule
        <- multiply
        |- divide
        |- number

    additive

let precedenceCalcExpressionWithParenthesis =
        let multiplicative = production "multiplicative"
        let additive = production "additive"
    
        let number = digits --> Number
        let parenthesis = ~~"(" +. additive .+ ~~")" --> id
        let factor = parenthesis |- number
    
        let add = additive .+ ~~"+" + multiplicative --> Add
        let sub = additive .+ ~~"-" + multiplicative --> Subtract
    
        let multiply = multiplicative .+ ~~"*" + factor --> Multiply
        let divide = multiplicative .+ ~~"/" + factor --> Divide
    
        additive.rule
            <- add
            |- sub
            |- multiplicative
    
        multiplicative.rule
            <- multiply
            |- divide
            |- factor
    
        additive
    


(* grammars used in the documentation / README.md *)

let twoDigitNumber = digit + digit --> fun (digit1, digit2) -> digit1 * 10 + digit2
let hello = ~~"Hello"
let oneOrTwo = oneOf "12"
let oneOrTwoAlt = (~~"1" |- ~~"2") --> fun str -> str.[0]

do
    let digit = oneOf "0123456789" --> fun c -> int(c) - int('0')
    let r = parse digit "3"
    match r with
    | Success s -> s.Value |> ignore
    | Failure _f -> failwith "error"

    |> ignore

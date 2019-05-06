module Ifmo.Compillers.LexicalAnalyzer.State

open System
open Ifmo.Compillers.LexicalAnalyzer.Tokens

let private isOperationSign ch =
    match ch with
        | '+' -> true
        | '-' -> true
        | '*' -> true
        | '/' -> true
        | '=' -> true
        | '<' -> true
        | '>' -> true
        | _ -> false

let private isWhitespace =
    Char.IsWhiteSpace

let private isDigit =
    Char.IsDigit

let private isLetter = 
    Char.IsLetter

let private isIdEnd ch = 
    ch |> isOperationSign || ch |> isWhitespace

let private isIdContinuation ch = 
    ch |> isDigit || ch |> isLetter

let private isOperationEnd ch =
    ch |> isWhitespace || ch |> isLetter || ch |> isDigit

type State = State of int | Final of Tokens.CreateAttributeToken | InvalidState

let private getNextStateOf0 ch = 
    match ch with
        | 'B' -> State(1)
        | 'E' -> State(6)
        | 'V' -> State(10)
        | 'I' -> State(16)
        | '=' -> State(14)
        | '*' -> State(18)
        | '(' -> State(20)
        | ')' -> State(21)
        | '-' -> State(22)
        | '+' -> State(23)
        | '>' -> State(25)
        | '<' -> State(26)
        | '/' -> State(28)
        | _ -> 
            if Char.IsWhiteSpace(ch) then
                State(0) 
            elif ch |> isLetter then 
                State(13)
            elif ch |> isDigit then
                State(19)
            else InvalidState
                   
let private getNextStateOf1 ch =
    match ch with   
        | 'e' -> State(2)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.id)
            else InvalidState

let private getNextStateOf2 ch = 
    match ch with   
        | 'g' -> State(3)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.id)
            else InvalidState

let private getNextStateOf3 ch = 
    match ch with   
        | 'i' -> State(4)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.id)
            else InvalidState

let private getNextStateOf4 ch = 
    match ch with   
        | 'n' -> State(5)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.id)
            else InvalidState

let private getNextStateOf5 ch = 
    if ch |> isWhitespace then 
        Final(Tokens.begin')
    elif ch |> isIdContinuation then
        State(13)
    else InvalidState

let private getNextStateOf6 ch = 
    match ch with   
        | 'n' -> State(7)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.id)
            else InvalidState

let private getNextStateOf7 ch = 
    match ch with   
        | 'd' -> State(8)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.id)
            else InvalidState

let private getNextStateOf8 ch = 
    match ch with   
        | '.' -> State(9)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.id)
            else InvalidState

let private getNextStateOf9 ch = 
    if ch |> isWhitespace then
        Final(end')
    else InvalidState

let private getNextStateOf10 ch = 
    match ch with   
        | 'a' -> State(11)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.id)
            else InvalidState

let private getNextStateOf11 ch = 
    match ch with   
        | 'r' -> State(12)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.id)
            else InvalidState

let private getNextStateOf12 ch = 
    if ch |> isWhitespace then 
        Final(Tokens.if')
    elif ch |> isIdContinuation then
        State(13)
    else InvalidState

let private getNextStateOf13 ch =
    if ch |> isWhitespace then
        Final(Tokens.id)
    elif ch |> isIdContinuation then
        State(13)
    else InvalidState

let private getNextStateOfOperation token ch =
    if ch |> isOperationEnd then
        Final(token)
    else InvalidState

let private getNextStateOf14 ch =
    if ch = '=' then
        State(27)
    else ch |> getNextStateOfOperation Tokens.assign

let private getNextStateOf16 ch = 
    if ch = 'F' then
        State(17)
    elif ch |> isIdContinuation then
        State(13)
    elif ch |> isIdEnd then
        Final(Tokens.id)
    else InvalidState

let private getNextStateOf17 ch =
    if ch |> isWhitespace || ch = '(' then
        Final(Tokens.if')
    elif ch |> isIdContinuation then
        State(13)
    else InvalidState

let private getNextStateOf18 ch =
    if ch = '*' then
        State(24)
    else ch |> getNextStateOfOperation Tokens.mul

let private getNextStateOf19 ch =
    if ch |> isDigit then
        State(19)
    elif ch |> isWhitespace || ch |> isOperationSign then
        Final(Tokens.const')
    else InvalidState

let private getNextStateOf20 ch = 
    ch |> getNextStateOfOperation Tokens.openBracket

let private getNextStateOf21 ch = 
    ch |> getNextStateOfOperation Tokens.closeBracket

let private getNextStateOf22 ch = 
    ch |> getNextStateOfOperation Tokens.minus

let private getNextStateOf23 ch = 
    ch |> getNextStateOfOperation Tokens.plus

let private getNextStateOf24 ch = 
    ch |> getNextStateOfOperation Tokens.pow

let private getNextStateOf25 ch = 
    ch |> getNextStateOfOperation Tokens.greater

let private getNextStateOf26 ch = 
    ch |> getNextStateOfOperation Tokens.less

let private getNextStateOf27 ch = 
    ch |> getNextStateOfOperation Tokens.equal

let private getNextStateOf28 ch = 
    ch |> getNextStateOfOperation Tokens.div

type GetNextState = State -> char -> State

let getNextState state ch =
    let matchNumberedState x =
        match x with 
            | 0 -> getNextStateOf0 
            | 1 -> getNextStateOf1
            | 2 -> getNextStateOf2
            | 3 -> getNextStateOf3
            | 4 -> getNextStateOf4
            | 5 -> getNextStateOf5
            | 6 -> getNextStateOf6
            | 7 -> getNextStateOf7
            | 8 -> getNextStateOf8
            | 9 -> getNextStateOf9
            | 10 -> getNextStateOf10
            | 11 -> getNextStateOf11
            | 12 -> getNextStateOf12
            | 13 -> getNextStateOf13
            | 14 -> getNextStateOf14
            | 16 -> getNextStateOf16
            | 17 -> getNextStateOf17
            | 18 -> getNextStateOf18
            | 19 -> getNextStateOf19
            | 20 -> getNextStateOf20
            | 21 -> getNextStateOf21
            | 22 -> getNextStateOf22
            | 23 -> getNextStateOf23
            | 24 -> getNextStateOf24
            | 25 -> getNextStateOf25
            | 26 -> getNextStateOf26
            | 27 -> getNextStateOf27
            | 28 -> getNextStateOf28
            | _ ->  fun _ -> InvalidState

    match state with
        | State x -> matchNumberedState x ch 
        | Final _ -> InvalidState
        | InvalidState -> state 


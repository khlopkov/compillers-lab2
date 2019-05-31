module Ifmo.Compillers.LexicalAnalyzer.State

open System
open Ifmo.Compillers.LexicalAnalyzer.Tokens

type State =
    | State of int
    | Final of Tokens.TokenWithAttr
    | InvalidState
    | Comment

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
    ch |> isOperationSign || ch |> isWhitespace || ch = ';' || ch = ','

let private isIdContinuation ch = 
    ch |> isDigit || ch |> isLetter

let private isOperationEnd ch =
    ch |> isWhitespace || ch |> isLetter || ch |> isDigit || ch = ';' || ch = ','

let private tokenToFinalState = 
    tokenToTokenWithAttr >> Final

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
        | ';' -> State(29)
        | ',' -> State(30)
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
                Final(Tokens.Id)
            else InvalidState

let private getNextStateOf2 ch = 
    match ch with   
        | 'g' -> State(3)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.Id)
            else InvalidState

let private getNextStateOf3 ch = 
    match ch with   
        | 'i' -> State(4)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.Id)
            else InvalidState

let private getNextStateOf4 ch = 
    match ch with   
        | 'n' -> State(5)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.Id)
            else InvalidState

let private getNextStateOf5 ch = 
    if ch |> isWhitespace then 
        tokenToFinalState Tokens.Begin
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
                Final(Tokens.Id)
            else InvalidState

let private getNextStateOf7 ch = 
    match ch with   
        | 'd' -> State(8)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.Id)
            else InvalidState

let private getNextStateOf8 ch = 
    match ch with   
        | '.' -> State(9)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.Id)
            else InvalidState

let private getNextStateOf9 ch = 
    if ch |> isWhitespace || ch |> isOperationSign then
        Final(tokenToTokenWithAttr End)
    else InvalidState

let private getNextStateOf10 ch = 
    match ch with   
        | 'a' -> State(11)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.Id)
            else InvalidState

let private getNextStateOf11 ch = 
    match ch with   
        | 'r' -> State(12)
        | _ ->
            if ch |> isIdContinuation then
                State(13)
            elif ch |> isIdEnd then
                Final(Tokens.Id)
            else InvalidState

let private getNextStateOf12 ch = 
    if ch |> isWhitespace then 
        tokenToFinalState Tokens.Var
    elif ch |> isIdContinuation then
        State(13)
    else InvalidState

let private getNextStateOf13 ch =
    if ch |> isIdEnd then
        Final(Tokens.Id)
    elif ch |> isIdContinuation then
        State(13)
    else InvalidState

let private getNextStateOfOperation token ch =
    if ch |> isOperationEnd then
        tokenToFinalState token
    else InvalidState

let private getNextStateOf14 ch =
    if ch = '=' then
        State(27)
    else getNextStateOfOperation Token.Assign ch

let private getNextStateOf16 ch = 
    if ch = 'F' then
        State(17)
    elif ch |> isIdContinuation then
        State(13)
    elif ch |> isIdEnd then
        Final(Tokens.Id)
    else InvalidState

let private getNextStateOf17 ch =
    if ch |> isWhitespace || ch = '(' then
        Tokens.tokenToTokenWithAttr >> Final <| Tokens.If
    elif ch |> isIdContinuation then
        State(13)
    else InvalidState

let private getNextStateOf18 ch =
    if ch = '*' then
        State(24)
    else ch |> getNextStateOfOperation Tokens.Mul

let private getNextStateOf19 ch =
    if ch |> isDigit then
        State(19)
    elif ch |> isWhitespace || ch |> isOperationSign then
        Tokens.Const >> tokenToFinalState <| 0
    else InvalidState

let private getNextStateOf20 ch = 
    ch |> getNextStateOfOperation Tokens.OpenBracket

let private getNextStateOf21 ch = 
    ch |> getNextStateOfOperation Tokens.CloseBracket

let private getNextStateOf22 ch = 
    ch |> getNextStateOfOperation Tokens.Minus

let private getNextStateOf23 ch = 
    ch |> getNextStateOfOperation Tokens.Plus

let private getNextStateOf24 ch = 
    ch |> getNextStateOfOperation Tokens.Pow

let private getNextStateOf25 ch = 
    ch |> getNextStateOfOperation Tokens.Greater

let private getNextStateOf26 ch = 
    ch |> getNextStateOfOperation Tokens.Less

let private getNextStateOf27 ch = 
    ch |> getNextStateOfOperation Tokens.Equal

let private getNextStateOf28 ch = 
    if ch = '/' then
        Comment
    else 
        ch |> getNextStateOfOperation Tokens.Div

let private getNextStateOf29 ch =
    ch |> getNextStateOfOperation Tokens.LineBreak
    
let private getNextStateOf30 ch =
    ch |> getNextStateOfOperation Tokens.Coma

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
            | 29 -> getNextStateOf29
            | 30 -> getNextStateOf30
            | _ ->  fun _ -> InvalidState

    match state with
        | State x -> matchNumberedState x ch 
        | Comment ->
            if ch = '\n' then
                State 0
            else 
                Comment
        | Final _ -> InvalidState
        | InvalidState -> state 


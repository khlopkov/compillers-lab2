module Ifmo.Compillers.LexicalAnalyzer.Tokens

type Token = 
    | Begin
    | End  
    | Var
    | If
    | Assign
    | Equal
    | Greater
    | Less
    | Plus
    | Minus
    | Mul
    | Div
    | Pow
    | Not
    | OpenBracket
    | CloseBracket
    | LineBreak
    | Coma
    | Id of string
    | Const of int

let compareByType a b =
    match a with
        | Id _ ->
            match b with
                | Id _ -> true
                | _ -> false
        | Const _ ->
            match b with
                | Const _ -> true
                | _ -> false
        | _ -> a = b
            
type TokenWithAttr = string -> Token

let tokenToTokenWithAttr (t: Token): TokenWithAttr =
    match t with
        | Id _ -> Id
        | Const _ -> int >> Const
        | _ -> fun _ -> t

let tokenToStr (t: Token) =
    match t with 
        | Begin -> "<begin>"
        | End -> "<end>"
        | Var -> "<var>"
        | If -> "<if>"
        | Assign -> "<assign>"
        | Equal -> "<equal>"
        | Greater -> "<greater>"
        | Less -> "<less>"
        | Plus -> "<plus>"
        | Minus -> "<minus>"
        | Mul -> "<mul>"
        | Div -> "<div>"
        | Pow -> "<pow>"
        | Not -> "<not>"
        | OpenBracket -> "<open_bracket>"
        | CloseBracket -> "<close_bracket>"
        | LineBreak -> "<line_break>"
        | Coma -> "<coma>"
        | Id name -> sprintf "<id, %s>" name
        | Const value -> sprintf "<const, 0x%08X>" value

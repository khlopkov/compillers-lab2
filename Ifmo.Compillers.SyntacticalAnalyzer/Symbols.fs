module Ifmo.Compillers.SyntacticalAnalyzer.Symbols
open Ifmo.Compillers.LexicalAnalyzer

type Symbol =
    | NonTerminal of string
    | Terminal of Tokens.Token

let bindTerminal (f: Tokens.Token -> Symbol) (s: Symbol): Symbol =
    match s with
        | Terminal t -> f t
        | _ -> s
        
let bindNonTerminal (f: string -> Symbol) (s: Symbol): Symbol =
    match s with
        | NonTerminal t -> f t
        | _ -> s
    
let program = NonTerminal "program"
let computationsDescribe = NonTerminal "computationDescribe"
let varDeclaration = NonTerminal "varDecl"
let varList = NonTerminal "varList"
let operatorList = NonTerminal "operatorList"
let operator = NonTerminal "operator"
let consistantOperator = NonTerminal "consistantOperator"
let expr = NonTerminal "expr"
let subExpr = NonTerminal "subExpr"
let assign = NonTerminal "assign"
let unaryOp = NonTerminal "unaryOp"
let binaryOp = NonTerminal "binaryOp"
let operand = NonTerminal "operand"
let complexOperator = NonTerminal "complexOperator"

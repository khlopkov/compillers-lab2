module Ifmo.Compillers.SyntacticalAnalyzer.Symbols
open Ifmo.Compillers.LexicalAnalyzer

type NonTerminal =
    | Program
    | ComputationsDescribe
    | VarDeclaration
    | VarList
    | OperatorList
    | Operator
    | ConsistantOperator
    | Expr
    | SubExpr
    | Assign
    | UnaryOp
    | BinaryOp
    | Operand
    | ComplexOperator  

type Symbol =
    | NonTerminal of NonTerminal
    | Terminal of Tokens.Token

let bindTerminal (f: Tokens.Token -> Symbol) (s: Symbol): Symbol =
    match s with
        | Terminal t -> f t
        | _ -> s
        
let bindNonTerminal (f: NonTerminal -> Symbol) (s: Symbol): Symbol =
    match s with
        | NonTerminal t -> f t
        | _ -> s

let nonTerminalSymbol =
    NonTerminal
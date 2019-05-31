module Ifmo.Compillers.SyntacticalAnalyzer.Rule
open Ifmo.Compillers.LexicalAnalyzer
open Ifmo.Compillers.SyntacticalAnalyzer.Symbols

type Rule = {
    left: Symbol
    right: list<Symbol>
}

let private findRulesByIdentifier (identifier: string): list<Rule> -> list<Rule> =
    fun rule ->
        match rule.left with
            | Symbols.NonTerminal x ->
                x = identifier
            | _ -> false
    |> List.filter
    
let rec first (rules: list<Rule>) (a: Symbol): Set<Symbol> =
    let foldFun (firstCallback: list<Rule> -> Symbol -> Set<Symbol>) (set: Set<Symbol>) (rule: Rule) =
        match List.head rule.right with 
            | Terminal x -> Terminal >> Set.empty.Add <| x
            | NonTerminal f -> Set.union(NonTerminal >> firstCallback rules <| f) set
    match a with
        | Terminal _ ->
            Set.empty.Add(a)
        | NonTerminal t ->
            List.fold (foldFun first) Set.empty (rules |> findRulesByIdentifier t)
            

let programRule = 
    {left = Symbols.program; right = [Symbols.varDeclaration; Symbols.computationsDescribe]};
let computationsDescribeRule =
    {left = Symbols.computationsDescribe; right=[
        Terminal Tokens.Begin; Symbols.operatorList; Terminal Tokens.End;
    ]}
let variableDeclaration =
    {left = Symbols.varDeclaration; right=[Terminal Tokens.Var; Symbols.varList]}
let variableList1 =
    {left = Symbols.varList; right=[Tokens.Id >> Terminal <| ""]}



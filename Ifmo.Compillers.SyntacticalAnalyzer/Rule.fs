module Ifmo.Compillers.SyntacticalAnalyzer.Rule
open Ifmo.Compillers.LexicalAnalyzer

type Rule = {
    left: Symbols.NonTerminal
    right: list<Symbols.Symbol>
}

type ConcreteRuleChooser = Symbols.Symbol -> List<Rule>
type RuleChooser = Symbols.NonTerminal -> ConcreteRuleChooser

let private getRule rulesByToken: ConcreteRuleChooser = fun s -> 
    let getToken =
        match s with
            | Symbols.Terminal l -> Some l
            | _ -> None
    let token = getToken
    match token with
    | None -> list.Empty
    | Some t ->
        rulesByToken t
        
let private getRuleOfConsistatOperator: ConcreteRuleChooser =
    let rulesByToken t =
        match t with
        | Tokens.If ->
            [{ left = Symbols.ConsistantOperator; right = [Symbols.Terminal Tokens.If;]}]
        | _ -> list.Empty
    getRule rulesByToken
 
let private getRuleOfBinaryOp: ConcreteRuleChooser =
    let oneTokenRule t =
        [{ left = Symbols.BinaryOp; right = [Symbols.Terminal t ]}]
    let rulesByToken t =
        //TODO: implement this
        match t with
        | Tokens.Minus ->
            oneTokenRule Tokens.Minus
        | _ -> list.Empty
    getRule rulesByToken

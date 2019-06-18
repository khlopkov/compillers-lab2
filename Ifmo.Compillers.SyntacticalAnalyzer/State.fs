module Ifmo.Compillers.SyntacticalAnalyzer.State
open Ifmo.Compillers.LexicalAnalyzer

type State = {
    root: Node.Node;
    position: int;
}

let initial root = {root = root; position = 0;}

let newState symbol position =
    { root = Node.root symbol;
    position = position}

let shift state currentToken = {
  root = Symbols.Terminal
      >> Node.root
      >> Node.addChild state.root
      <| currentToken;
  position = state.position + 1}

let mergeWithChild parent child =
    { root = Node.addChild parent.root child.root; position = child.position}

let failedState = {root = Node.Failure; position = 0}

let isSuccessfull = fun s ->
    match s.root with
    | Node.Node _ -> true
    | Node.Failure -> false

let chooseWithGreatestPosition (l: State list) =
    List.reduce
    <| (fun a b -> if a.position >= b.position then a else b)
    <| l
    
let rec stateByNonTerminal state nonTerminal (lexems: Tokens.Token list) =
    let currentToken =
        lexems.Item(state.position)
    let foldFun state s =
        let currentToken =
            lexems.Item(state.position)
        match state.root with
        | Node.Failure -> failedState
        | _ ->    
            match s with
            | Symbols.Terminal t ->
                if Tokens.compareByType t currentToken then
                    shift state currentToken
                else
                    mergeWithChild state failedState
            | Symbols.NonTerminal n ->
                let child = stateByNonTerminal(newState s state.position) n lexems
                if not <| isSuccessfull child then
                    mergeWithChild state failedState
                else 
                    mergeWithChild state child
            
    let foldRule (state: State) (rule: Rule.Rule) =
        List.fold foldFun state rule.right 
            
    let rule = Rule.getRulesByNonTerminal nonTerminal currentToken
        
    if state.position > lexems.Length - 1 then
        failedState
    else
        match rule with
        | None -> failedState
        | Some r -> foldRule state r
        
let parse (lexems: Tokens.Token list) =
    let programNode = Symbols.NonTerminal >> Node.root <| Symbols.Program
    initial
    >> stateByNonTerminal
    <| programNode
    <| Symbols.Program
    <| lexems

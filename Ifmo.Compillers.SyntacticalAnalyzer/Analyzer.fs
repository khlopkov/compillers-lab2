module Ifmo.Compillers.SyntacticalAnalyzer.Analyzer

type AnalyzerState = {
    node: Nodes.Node
    currentSymbol: int
}
let private failedState state =
    {node = Nodes.Failure; currentSymbol = state.currentSymbol}

let private shift state =
    {node = state.node; currentSymbol = state.currentSymbol + 1}
    
// 
// let abstractNodeParser (buffer: string) (state: AnalyzerState): AnalyzerState =
//     if buffer.Length = state.currentSymbol + 1 then
//         if state.stack.Length = 0 then
//             state
//         else
//             failedState state
//     elif state.stack.Length = 0 then
//         {stack = state.stack; node = state.node; currentSymbol = state.currentSymbol}
//     else
//         match state.node with
//             | Failure -> failedState state
//             //TODO: implement
//             | Node (leaf, childs) -> failedState state
//     
//     

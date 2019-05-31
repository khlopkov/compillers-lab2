module Ifmo.Compillers.SyntacticalAnalyzer.Analyzer

// type Node =
//     | Node of (Symbols.Symbol * list<Node>)
//     | Failure 
// 
// type AnalyzerState = {
//     stack: list<Symbols.Symbol>
//     node: Node
//     currentSymbol: int
// }
// 
// let (>>=) (f: (Symbols.Symbol * list<Node>) -> Node) (node: Node) : Node =
//     match node with
//         | Node x -> f x
//         | Failure -> Failure
// 
// let private failedState state =
//     {stack = state.stack; node = state.node; currentSymbol = state.currentSymbol}
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

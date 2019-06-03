module Ifmo.Compillers.SyntacticalAnalyzer.Nodes

type Node =
    | Node of (Symbols.Symbol * list<Node>)
    | Failure
    
let (>>=) (f: (Symbols.Symbol * list<Node>) -> Node) (node: Node) : Node =
    match node with
        | Node x -> f x
        | Failure -> Failure

let addChild child parrent =
    (fun (s, l) ->
        (s, List.append l [child])
        |> Node)
    >>= parrent

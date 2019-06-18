module Ifmo.Compillers.SyntacticalAnalyzer.Node
open Ifmo.Compillers.SyntacticalAnalyzer

type Node =
    | Node of (Symbols.Symbol * list<Node>)
    | Failure
    
let (>>=) (f: (Symbols.Symbol * list<Node>) -> Node) (node: Node) : Node =
    match node with
        | Node x -> f x
        | Failure -> Failure

let addChild parrent child =
    (fun (s, l) ->
        (s, List.append l [child])
        |> Node)
    >>= parrent

let root s = Node (s, List.empty);

let rec toJson node =
    let childrenToArray children =
        (List.map (fun c -> toJson c) 
        >> List.toSeq
        >> String.concat ", "
        >> (+) "["
        <| children) + "]"
    match node with
    | Node (s, children) ->
        "{ \"node\": \"" + (Symbols.toString s).Replace("\"", "'") + "\", \"children\": " + childrenToArray children + "}"
    | Failure -> "{ \"node\": \"Failure\" }"
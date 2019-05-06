module Ifmo.Compillers.LexicalAnalyzer.Analyzer

//TODO add comments deletion

open Ifmo.Compillers.LexicalAnalyzer.Tokens
open Ifmo.Compillers.LexicalAnalyzer.State

type Analyze = string -> list<Token>

let private getStateFromBuffer (buff:string): State = 
    let charList = Seq.toList buff
    List.fold getNextState (State 0) charList 

let private delWs str =
    String.filter (fun ch ->
        System.Char.IsWhiteSpace ch
        |> Operators.not
    ) str

let private foldStringToLexems (str: string) =
    let foldFun (
                 state: State,
                 lexems: list<Token>,
                 buffer: string,
                 lexemBegin: int,
                 forward: int
         ) (ch: char) =
        let getState = getNextState state ch
        match getState with  
            | State(x) ->
                (State(x), lexems, buffer + string ch, lexemBegin, forward+1)
            | Final(t) ->
                (getNextState (State 0) ch, List.append lexems [buffer |> delWs |> t],
                 string ch, forward + 1, forward + 1)
            | InvalidState ->
                (InvalidState, lexems, "", lexemBegin, forward + 1)
    str |> Seq.toList |> List.fold foldFun (State(0), List.empty<Token>, "", 0, 0)

let analyze input =
    let inputWithWS = input + " "
    let (state, lexems, buffer, lastSuccessfulLexem, _) = foldStringToLexems inputWithWS
    match state with
        | State(x) ->
            if x <> 0 then
                failwith <| sprintf "error at %i" lastSuccessfulLexem
            else
                lexems
        | _ -> failwith <| sprintf "error at %i" lastSuccessfulLexem

module Ifmo.Compillers.LexicalAnalyzer.Analyzer

//TODO add comments deletion

open Ifmo.Compillers.LexicalAnalyzer.Tokens
open Ifmo.Compillers.LexicalAnalyzer.State

type Analyze = string -> list<Token>

let private getStateFromBuffer (buff:string): State = 
    let charList = Seq.toList buff
    List.fold getNextState (State 0) charList 


type private AnalyzerState = {
     state: State;
     lexems: list<Token>;
     forwardBuff: string;
     lexemBegin: int;
     forward: int
 }

let private getNextAnalyzerState (st: AnalyzerState) (ch: char) : AnalyzerState = 
    let delWs str =
        str |> String.filter (fun ch ->
            ch 
            |> System.Char.IsWhiteSpace 
            |> Operators.not) 
    let state = getNextState st.state ch
    match state with
        | State(x) ->
             {state = State x;
             lexems = st.lexems;
             forwardBuff = st.forwardBuff + string ch;
             lexemBegin = st.lexemBegin;
             forward=st.forward+1}
        | Final(t) ->
             {state= 0 |> State |> getNextState <| ch;
             lexems = List.append st.lexems [st.forwardBuff |> delWs |> t];
             forwardBuff = string ch;
             lexemBegin = st.forward + 1;
             forward = st.forward + 1}
        | InvalidState ->
             {state=InvalidState;
             lexems = st.lexems;
             forwardBuff = "";
             lexemBegin = st.lexemBegin;
             forward = st.forward + 1}
            
let private foldStringWithAnalyzer (str: string) =
   Seq.toList str 
   |> List.fold getNextAnalyzerState
        {state = State 0;
        lexems = List.empty<Token>;
        forwardBuff = "";
        lexemBegin = 0;
        forward = 0}
       
let private analysisResult onSuccess onError (resState: AnalyzerState) =
    let err = onError resState.lexemBegin
    match resState.state with
        | State(x) ->
            if x <> 0 then
                err
            else
                onSuccess resState.lexems
        | _ -> err
    
type AnalysisResult =
    | SuccessResult of list<Token> 
    | FailedAt of int

let analyze input =
    let inputWithWS = input + " "
    let resultState = foldStringWithAnalyzer inputWithWS
    analysisResult SuccessResult FailedAt resultState


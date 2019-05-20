module Ifmo.Compillers.LexicalAnalyzer.Analyzer

open Ifmo.Compillers.LexicalAnalyzer.Tokens
open Ifmo.Compillers.LexicalAnalyzer.State
open System


type TokenWithPosition = {
    token: Token;
    position: int * int;
} with
    member this.ToString =
        let strToken = tokenToStr this.token
        match this.position with
            | (row, col) -> sprintf "%s [%d:%d]" strToken row col

type Analyze = string -> list<TokenWithPosition>

type private AnalyzerState = {
     state: State;
     lexems: list<TokenWithPosition>;
     forwardBuff: string;
     lexemeBeginPosition: int * int;
     forwardPosition: int* int;
 }

let private getNextAnalyzerState (st: AnalyzerState) (ch: char) : AnalyzerState = 
    let state = getNextState st.state ch
    let addToForwardBuff =
        if ch |> Char.IsWhiteSpace then
            st.forwardBuff
        else
            st.forwardBuff + string ch
    let getNextPosition = 
        match st.forwardPosition with (row, col) ->
            if ch = '\n' then
                (row + 1, 1)
            else 
                (row, col + 1)
    match state with
        | State(x) ->
            let getLexemeBeginPosition = 
                if x = 0 && ch |> Char.IsWhiteSpace then
                    st.forwardPosition;
                else 
                    st.lexemeBeginPosition;
            {state = State x;
            lexems = st.lexems;
            forwardBuff = addToForwardBuff;
            lexemeBeginPosition = getLexemeBeginPosition;
            forwardPosition = getNextPosition}
        | Comment ->
            {state = Comment;
            lexems = st.lexems;
            forwardBuff = "";
            lexemeBeginPosition = getNextPosition;
            forwardPosition = getNextPosition}
        | Final(t) ->
             let refreshForwardBuff = 
                if ch |> Char.IsWhiteSpace then
                    ""
                else 
                    string ch;
             {state= 0 |> State |> getNextState <| ch;
             lexems = List.append st.lexems [{token = st.forwardBuff |> t; position = st.lexemeBeginPosition}];
             forwardBuff = refreshForwardBuff;
             lexemeBeginPosition = getNextPosition;
             forwardPosition = getNextPosition}
        | InvalidState ->
             {state=InvalidState;
             lexems = st.lexems;
             forwardBuff = "";
             lexemeBeginPosition = st.lexemeBeginPosition;
             forwardPosition = getNextPosition}
            
let private foldStringWithAnalyzer (str: string) =
   Seq.toList str 
   |> List.fold getNextAnalyzerState
        {state = State 0;
        lexems = List.empty<TokenWithPosition>;
        forwardBuff = "";
        lexemeBeginPosition = (1, 1);
        forwardPosition = (1, 1)}
       
let private analysisResult onSuccess onError (resState: AnalyzerState) =
    let err = onError resState.lexemeBeginPosition
    match resState.state with
        | State(x) ->
            if x <> 0 then
                err
            else
                onSuccess resState.lexems
        | _ -> err
    
type AnalysisResult =
    | SuccessResult of list<TokenWithPosition> 
    | FailedAt of int * int

let analyze input =
    let inputWithWS = input + " "
    let resultState = foldStringWithAnalyzer inputWithWS
    analysisResult SuccessResult FailedAt resultState


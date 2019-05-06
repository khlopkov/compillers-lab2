module Ifmo.Compillers.LexicalAnalyzer.Analyzer

//TODO add comments deletion

open Ifmo.Compillers.LexicalAnalyzer.Tokens
open Ifmo.Compillers.LexicalAnalyzer.State

type Analyze = string -> list<Token>

let private getStateFromBuffer (buff:string): State = 
    let charList = Seq.toList buff
    List.fold getNextState (State 0) charList 

//TODO:
//  try to use less mutability
//  decompose
let analyze (input: string): list<Token> = 
    let mutable lexemeBegin = 0
    let mutable forward = 0
    let mutable state = State(0)
    let mutable lexems = List.empty<Token>
    let inputWithWS = input + " "

    let failMessage = 
        sprintf "parse error at %i" lexemeBegin

    do while forward <> inputWithWS.Length+1 do
        let forwardBuffer = inputWithWS.[lexemeBegin..forward-1]
            
        do state <- getStateFromBuffer forwardBuffer
        do match state with
            | State(x) -> 
                if inputWithWS.Length = forward && x <> 0 then
                    failwith failMessage
                else
                    forward <- forward + 1

            | Final t ->
                let createToken (t: CreateAttributeToken) =
                    let buffer = inputWithWS.[lexemeBegin..forward-1]
                    let delWhiteSpaces str = 
                        let notWs = System.Char.IsWhiteSpace >> Operators.not
                        String.filter notWs str
                    t (delWhiteSpaces buffer)
                lexems <- List.append lexems [createToken t]
                do lexemeBegin <- forward 

            | InvalidState -> failwith failMessage
    lexems

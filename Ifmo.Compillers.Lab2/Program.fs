// Learn more about F# at http://fsharp.org

open System
open Ifmo.Compillers.LexicalAnalyzer
open Ifmo.Compillers.LexicalAnalyzer.Analyzer
open Ifmo.Compillers.SyntacticalAnalyzer

let symbolsToString (table: Map<string, Tokens.Token>) =
    let tokensOfTable: list<Tokens.Token> = List.map (fun (_, v) -> v)  (Map.toList table)
    List.map Tokens.tokenToStr tokensOfTable 
    

let tokenListToString (list: list<TokenWithPosition>) = 
    let tokenToStr (t: TokenWithPosition) = t.ToString
    List.map tokenToStr list

[<EntryPoint>]
let main argv =
    let res = Analyzer.analyze """Var
    a, b ; a ;
    Begin
        a = b + c ;
    End.
    """
    match res with
        | Analyzer.SuccessResult(tokens, symbolsTable) ->
            do tokens |> tokenListToString |> printfn "Lexems list : %A \n"
            do symbolsTable |> symbolsToString |> printfn "Symbols table: %A \n"
            do Node.toJson
               >> fun content -> System.IO.File.WriteAllText("data.json", content)
               <| (List.map (fun t -> t.token) 
                   >> State.parse
                   <| tokens).root
        | Analyzer.FailedAt(row,  col) ->
            printfn "failed at position %d %d" row col
    Console.ReadKey() |> ignore
    
    0 // return an integer exit code

// Learn more about F# at http://fsharp.org

open System
open Ifmo.Compillers.LexicalAnalyzer
open Ifmo.Compillers.LexicalAnalyzer.Analyzer

let tokenListToString (list: list<TokenWithPosition>) = 
    let tokenToStr (t: TokenWithPosition) = t.ToString
    List.map tokenToStr list

[<EntryPoint>]
let main argv =
    let res = Analyzer.analyze """
        Begin
        End.
        Var
        IF
        kek
        kek-kek
        kek - kek
        kek*kek
        kek * kek
        kek ** kek
    """
    match res with
        | Analyzer.SuccessResult(tokens) ->
            tokens |> tokenListToString |> printfn "%A" 
        | Analyzer.FailedAt(row,  col) ->
            printfn "failed at position %d %d" row col
    Console.ReadKey() |> ignore
    0 // return an integer exit code

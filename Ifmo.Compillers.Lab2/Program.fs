// Learn more about F# at http://fsharp.org

open System
open Ifmo.Compillers.LexicalAnalyzer
open Ifmo.Compillers.LexicalAnalyzer.Tokens

let tokenListToString (list: list<Token>) = 
    let tokenToStr (t: Token) = t.ToString
    List.map tokenToStr list

[<EntryPoint>]
let main argv =
    let res = Analyzer.analyze "
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
    "
    match res with
        | Analyzer.SuccessResult(tokens) ->
            tokens |> tokenListToString |> printfn "%A" 
        | Analyzer.FailedAt(failurePos) ->
            printfn "failed at position %d" failurePos
    Console.ReadKey() |> ignore
    0 // return an integer exit code

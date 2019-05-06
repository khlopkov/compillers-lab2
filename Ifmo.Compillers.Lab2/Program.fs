// Learn more about F# at http://fsharp.org

open System
open Ifmo.Compillers.LexicalAnalyzer
open Ifmo.Compillers.LexicalAnalyzer.Tokens

let tokenListToString (list: list<Token>) = 
    let tokenToStr (t: Token) = t.ToString
    List.map tokenToStr list

[<EntryPoint>]
let main argv =
    let tokens = Analyzer.analyze "Begin End. Var kek"
    printfn "%A" <| tokenListToString tokens
    Console.ReadKey()
    0 // return an integer exit code

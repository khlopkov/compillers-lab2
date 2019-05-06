module Ifmo.Compillers.LexicalAnalyzer.Token

type Token = {name: string; attribute: Option<string>} with
    member this.ToString =
        let concatPostfix postfix = "<" + this.name + postfix
        match this.attribute with
            | None -> concatPostfix ">"
            | _ -> ", " + this.attribute.Value + ">" |> concatPostfix
    
let private tokenWithoutAttr tokenName = { name=tokenName; attribute=None }

let begin' = tokenWithoutAttr "begin"
let end' = tokenWithoutAttr "end"
let var = tokenWithoutAttr "var"
let if' = tokenWithoutAttr "if"

let assign = tokenWithoutAttr "assign"

let equal = tokenWithoutAttr "eq"
let greater = tokenWithoutAttr "gt"
let less = tokenWithoutAttr "less"

let plus = tokenWithoutAttr "plus"
let minus = tokenWithoutAttr "minus"
let mul = tokenWithoutAttr "mul"
let div = tokenWithoutAttr "div"

let pow = tokenWithoutAttr "pow"
let not = tokenWithoutAttr "not"

let openBracket = tokenWithoutAttr "open-bracket"
let closeBracket = tokenWithoutAttr "close-bracket"

let id idName = { name="id"; attribute=idName }
let const' val' = { name="const"; attribute=val' }

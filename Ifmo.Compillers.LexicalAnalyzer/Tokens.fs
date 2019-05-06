module Ifmo.Compillers.LexicalAnalyzer.Tokens

type Token = {name: string; attribute: Option<string>} with
    member this.ToString =
        let concatPostfix postfix = "<" + this.name + postfix
        match this.attribute with
            | None -> concatPostfix ">"
            | _ -> ", " + this.attribute.Value + ">" |> concatPostfix
    
let private tokenWithoutAttr tokenName = { name=tokenName; attribute=None }

type CreateAttributeToken = string -> Token

let begin' _  = tokenWithoutAttr "begin"
let end' _  = tokenWithoutAttr "end"
let var _ = tokenWithoutAttr "var"
let if' _ = tokenWithoutAttr "if"

let assign _ = tokenWithoutAttr "assign"

let equal _ = tokenWithoutAttr "eq"
let greater _ = tokenWithoutAttr "gt"
let less _  = tokenWithoutAttr "less"

let plus _  = tokenWithoutAttr "plus"
let minus _  = tokenWithoutAttr "minus"
let mul _  = tokenWithoutAttr "mul"
let div _  = tokenWithoutAttr "div"

let pow _  = tokenWithoutAttr "pow"
let not _  = tokenWithoutAttr "not"

let openBracket _  = tokenWithoutAttr "open-bracket"
let closeBracket _  = tokenWithoutAttr "close-bracket"


let id idName = { name="id"; attribute=Some(idName) }
let const' val' = { name="const"; attribute=Some(val') }

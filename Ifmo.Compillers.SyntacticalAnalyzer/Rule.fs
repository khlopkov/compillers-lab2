module Ifmo.Compillers.SyntacticalAnalyzer.Rule

type Rule = {
    left: Symbols.NonTerminal
    right: list<Symbols.Symbol>
}

        
module private RuleDefs =
    open Ifmo.Compillers.LexicalAnalyzer
    type RuleDef = Symbols.Symbol -> List<Rule>
    
    let private getRule rulesByToken: RuleDef = fun s -> 
        let getToken =
            match s with
                | Symbols.Terminal l -> Some l
                | _ -> None
        let token = getToken
        match token with
        | None -> list.Empty
        | Some t ->
            rulesByToken t
            
    let private oneSymbolRule left right =
        { left = left; right = [right]}
        
    let private oneTokenRule = fun left ->
        Symbols.Terminal
        >> oneSymbolRule left
        
        
    let getRuleOfConsistatOperator: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.If ->
                [{ left = Symbols.ConsistantOperator; right = [
                    Symbols.Terminal Tokens.If;
                    Symbols.Terminal Tokens.OpenBracket;
                    Symbols.NonTerminal Symbols.Expr;
                    Symbols.Terminal Tokens.CloseBracket;
                    Symbols.NonTerminal Symbols.Operator;
                ]}]
            | _ -> list.Empty
        getRule rulesByToken
 
        
    let getRuleOfBinaryOp: RuleDef =
        let oneTokenRule t =
            [{ left = Symbols.BinaryOp; right = [Symbols.Terminal t ]}]
        let rulesByToken t =
            match t with
            | Tokens.Minus 
            | Tokens.Plus 
            | Tokens.Mul
            | Tokens.Div
            | Tokens.Pow
            | Tokens.Greater
            | Tokens.Less
            | Tokens.Equal ->
                oneTokenRule t
            | _ -> list.Empty
        getRule rulesByToken
        
    let getRuleOfUnaryOp: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Minus 
            | Tokens.Not ->
                [oneTokenRule Symbols.UnaryOp t]
            | _ -> list.Empty
        getRule rulesByToken
        
    let getRuleOfSubexpr: RuleDef =
        let commonRule = {left = Symbols.SubExpr; right = [
            Symbols.NonTerminal Symbols.SubExpr;
            Symbols.NonTerminal Symbols.BinaryOp;
            Symbols.NonTerminal Symbols.SubExpr;
        ]}
        let rulesByToken t =
            match t with
            | Tokens.OpenBracket ->
                [ { left = Symbols.SubExpr;
                    right = [ Symbols.Terminal Tokens.OpenBracket;
                    Symbols.NonTerminal Symbols.Expr;
                    Symbols.Terminal Tokens.CloseBracket; ]
                 }; commonRule ]
            | Tokens.Id _
            | Tokens.Const _ ->
                Symbols.NonTerminal
                >> (oneSymbolRule Symbols.SubExpr)
                >> (fun r ->  [r; commonRule] )
                <| Symbols.Operand
                
            | _ -> list.Empty
        getRule rulesByToken
    
    let getRuleOfExpr: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Minus
            | Tokens.Not ->
                [ { left = Symbols.Expr;
                    right = [
                        Symbols.NonTerminal Symbols.UnaryOp;
                        Symbols.NonTerminal Symbols.SubExpr; ] } ]
            | Tokens.OpenBracket
            | Tokens.Id _
            | Tokens.Const _ ->
                [ { left = Symbols.Expr;
                    right = [
                        Symbols.NonTerminal Symbols.SubExpr; ] } ]
            | _ -> list.Empty
        getRule rulesByToken
     
    let getRuleOfAssign: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _ ->
                [ { left = Symbols.Assign;
                    right = [
                        Tokens.Id >> Symbols.Terminal <| "";
                        Symbols.Terminal Tokens.Assign;
                        Symbols.NonTerminal Symbols.Expr ] } ]
            | _ -> list.Empty
        getRule rulesByToken
                
    let getRuleOfConsistantOperator: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Begin _ ->
                [ { left = Symbols.ConsistantOperator;
                    right = [
                        Symbols.Terminal Tokens.Begin;
                        Symbols.NonTerminal Symbols.OperatorList;
                        Symbols.Terminal Tokens.End ] } ]
            | _ -> list.Empty
        getRule rulesByToken
        
    let getRuleOfOperator: RuleDef =
        let oneSymbolRule' = 
            Symbols.NonTerminal
            >> (oneSymbolRule <| Symbols.Operator)
        let rulesByToken t =
            match t with
            | Tokens.Id _ ->
                [oneSymbolRule' Symbols.Assign]
            | Tokens.Begin ->
                [oneSymbolRule' Symbols.ConsistantOperator]
            | Tokens.If ->
                [oneSymbolRule' Symbols.ComplexOperator]
            | _ -> list.Empty
        getRule rulesByToken
    
    let getRuleOfOperatorList: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _
            | Tokens.If
            | Tokens.Begin ->
                Symbols.NonTerminal
                >> (oneSymbolRule <| Symbols.OperatorList)
                >> fun r -> [r; {
                    left = Symbols.OperatorList;
                    right = [
                        Symbols.NonTerminal Symbols.Operator;
                        Symbols.NonTerminal Symbols.OperatorList;
                    ] }]
                <| Symbols.Operator
            | _ -> list.Empty
        getRule rulesByToken
        
    let getRuleOfVarList: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _ -> 
                (oneTokenRule <| Symbols.VarList)
                >> fun r -> [r; {
                    left = Symbols.VarList;
                        right = [
                        Symbols.Terminal t;
                        Symbols.Terminal Tokens.Coma;
                        Symbols.NonTerminal Symbols.VarList;
                    ] }; { left = Symbols.VarList;
                           right = [
                        Symbols.Terminal t;
                        Symbols.Terminal Tokens.LineBreak;
                        Symbols.NonTerminal Symbols.VarList;
                    ] }]
                <| t
            | _ -> list.Empty
        getRule rulesByToken
        
    let getRuleOfVarDeclaration: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Var _ -> [{
                left = Symbols.VarDeclaration;
                right = [
                    Symbols.Terminal Tokens.Var;
                    Symbols.NonTerminal Symbols.VarList;
                ] }]
            | _ -> list.Empty
        getRule rulesByToken
        
    let getRuleOfComputationsDescribe: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Begin _ -> [{
                left = Symbols.ComputationsDescribe;
                right = [
                    Symbols.Terminal Tokens.Begin;
                    Symbols.NonTerminal Symbols.OperatorList;
                    Symbols.Terminal Tokens.End;
                ] }]
            | _ -> list.Empty
        getRule rulesByToken
        
    let getRuleOfProgram: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Var _ -> [{
                left = Symbols.Program;
                right = [
                    Symbols.NonTerminal Symbols.VarDeclaration;
                    Symbols.NonTerminal Symbols.ComputationsDescribe;
                ] }]
            | _ -> list.Empty
        getRule rulesByToken
        
type RuleChooser = Symbols.NonTerminal -> RuleDefs.RuleDef



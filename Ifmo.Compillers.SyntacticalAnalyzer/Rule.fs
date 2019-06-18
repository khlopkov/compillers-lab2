module Ifmo.Compillers.SyntacticalAnalyzer.Rule

type Rule = {
    left: Symbols.NonTerminal
    right: list<Symbols.Symbol>
}

let epsilonRule n = {
    left = n; right = []
}

module private RuleDefs =
    open Ifmo.Compillers.LexicalAnalyzer
    type RuleDef = Tokens.Token -> Rule Option
    
    let private oneSymbolRule left right =
        { left = left; right = [right]}
        
    let private oneTokenRule = fun left ->
        Symbols.Terminal
        >> oneSymbolRule left
        
        
    let getRuleOfComplexOperator: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.If ->
                Some { left = Symbols.ComplexOperator; right = [
                    Symbols.Terminal Tokens.If;
                    Symbols.Terminal Tokens.OpenBracket;
                    Symbols.NonTerminal Symbols.Expr;
                    Symbols.Terminal Tokens.CloseBracket;
                    Symbols.NonTerminal Symbols.Operator;
                ]}
            | _ -> None
        rulesByToken
 
        
    let getRuleOfBinaryOp: RuleDef =
        let oneTokenRule t =
            Some { left = Symbols.BinaryOp; right = [Symbols.Terminal t ]}
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
            | _ -> None
        rulesByToken
        
    let getRuleOfOperand: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _
            | Tokens.Const _ ->
                oneTokenRule Symbols.Operand
                >> Some <| t
            | _ -> None
        rulesByToken
        
    let getRuleOfUnaryOp: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Minus 
            | Tokens.Not ->
                oneTokenRule Symbols.UnaryOp
                >> Some <| t
            | _ -> None
        rulesByToken
        
    let getRuleOfSubExpr: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.OpenBracket ->
                Some { left = Symbols.SubExpr; right = [
                    Symbols.Terminal Tokens.OpenBracket;
                    Symbols.NonTerminal Symbols.Expr;
                    Symbols.Terminal Tokens.CloseBracket;
                    Symbols.NonTerminal Symbols.SubExpr' ] }
            | Tokens.Id _
            | Tokens.Const _ ->
                Some { left = Symbols.SubExpr; right = [
                    Symbols.NonTerminal Symbols.Operand;
                    Symbols.NonTerminal Symbols.SubExpr'; ] }
                
            | _ -> None
        rulesByToken
    
    let getRuleOfSubExpr': RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Equal
            | Tokens.Greater
            | Tokens.Less
            | Tokens.Pow
            | Tokens.Div
            | Tokens.Mul
            | Tokens.Minus
            | Tokens.Plus ->
                Some {left = Symbols.SubExpr'; right = [
                    Symbols.NonTerminal Symbols.BinaryOp;
                    Symbols.NonTerminal Symbols.SubExpr;
                ]}
            | _ -> epsilonRule >> Some <| Symbols.SubExpr' 
        rulesByToken
        
    let getRuleOfExpr: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Minus
            | Tokens.Not ->
                Some { left = Symbols.Expr; right = [
                    Symbols.NonTerminal Symbols.UnaryOp;
                    Symbols.NonTerminal Symbols.SubExpr; ] }
            | Tokens.OpenBracket
            | Tokens.Id _
            | Tokens.Const _ ->
                Some { left = Symbols.Expr; right = [
                    Symbols.NonTerminal Symbols.SubExpr; ] }
            | _ -> None
        rulesByToken
     
    let getRuleOfAssign: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _ ->
                Some { left = Symbols.Assign; right = [
                    Tokens.Id >> Symbols.Terminal <| "";
                    Symbols.Terminal Tokens.Assign;
                    Symbols.NonTerminal Symbols.Expr;
                    Symbols.Terminal Tokens.LineBreak ] }
            | _ -> None
        rulesByToken
                
    let getRuleOfConsistantOperator: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Begin _ ->
                Some { left = Symbols.ConsistantOperator; right = [
                    Symbols.Terminal Tokens.Begin;
                    Symbols.NonTerminal Symbols.OperatorList;
                    Symbols.Terminal Tokens.End ] }
            | _ -> None
        rulesByToken
        
    let getRuleOfOperator: RuleDef =
        let oneSymbolRule' = 
            Symbols.NonTerminal
            >> (oneSymbolRule <| Symbols.Operator)
        let rulesByToken t =
            match t with
            | Tokens.Id _ ->
                Some <| oneSymbolRule' Symbols.Assign
            | Tokens.Begin ->
                Some <| oneSymbolRule' Symbols.ConsistantOperator
            | Tokens.If ->
                Some <| oneSymbolRule' Symbols.ComplexOperator
            | _ -> None
        rulesByToken
    
    let getRuleOfOperatorList: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _
            | Tokens.If
            | Tokens.Begin ->
                Some {left = Symbols.OperatorList; right = [
                    Symbols.NonTerminal Symbols.Operator;
                    Symbols.NonTerminal Symbols.OperatorList'
                ]}
            | _ -> None
        rulesByToken
        
    let getRuleOfOperatorList': RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _
            | Tokens.If
            | Tokens.Begin ->
                 Symbols.NonTerminal
                >> (oneSymbolRule <| Symbols.OperatorList')
                >> Some
                <| Symbols.OperatorList 
            | _ ->  Some <| epsilonRule Symbols.OperatorList' 
        rulesByToken
        
    let getRuleOfVarList: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _ -> 
                Some { left = Symbols.VarList; right = [
                    Symbols.Terminal t;
                    Symbols.NonTerminal Symbols.VarList';
                ] }
            | _ -> None
        rulesByToken
        
    let getRuleOfVarList': RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.LineBreak _ -> 
                Some{ left = Symbols.VarList'; right = [
                    Symbols.Terminal Tokens.LineBreak;
                    Symbols.NonTerminal Symbols.VarList''; ] }
            | Tokens.Coma _ -> 
                Some { left = Symbols.VarList'; right = [
                    Symbols.Terminal Tokens.Coma;
                    Symbols.NonTerminal Symbols.VarList; ] }
            | _ -> None
        rulesByToken
        
    let getRuleOfVarList'': RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _ -> 
                Some { left = Symbols.VarList''; right = [ Symbols.NonTerminal Symbols.VarList; ] }
            | _ -> Some <| epsilonRule Symbols.VarList''
        rulesByToken
        
    let getRuleOfVarDeclaration: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Var _ ->
                Some { left = Symbols.VarDeclaration; right = [
                    Symbols.Terminal Tokens.Var;
                    Symbols.NonTerminal Symbols.VarList; ] }
            | _ -> None
        rulesByToken
        
    let getRuleOfComputationsDescribe: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Begin _ ->
                Some { left = Symbols.ComputationsDescribe; right = [
                    Symbols.Terminal Tokens.Begin;
                    Symbols.NonTerminal Symbols.OperatorList;
                    Symbols.Terminal Tokens.End; ] }
            | _ -> None
        rulesByToken
        
    let getRuleOfProgram: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Var _ ->
                Some { left = Symbols.Program; right = [
                    Symbols.NonTerminal Symbols.VarDeclaration;
                    Symbols.NonTerminal Symbols.ComputationsDescribe;
                ] }
            | _ -> None 
        rulesByToken
        
type RuleChooser = Symbols.NonTerminal -> RuleDefs.RuleDef


let getRulesByNonTerminal left currentToken = 
    match left with
    | Symbols.Program -> RuleDefs.getRuleOfProgram currentToken
    | Symbols.ComputationsDescribe -> RuleDefs.getRuleOfComputationsDescribe currentToken
    | Symbols.VarDeclaration -> RuleDefs.getRuleOfVarDeclaration currentToken
    | Symbols.VarList -> RuleDefs.getRuleOfVarList currentToken
    | Symbols.VarList' -> RuleDefs.getRuleOfVarList' currentToken
    | Symbols.VarList'' -> RuleDefs.getRuleOfVarList'' currentToken
    | Symbols.OperatorList -> RuleDefs.getRuleOfOperatorList currentToken
    | Symbols.OperatorList' -> RuleDefs.getRuleOfOperatorList' currentToken
    | Symbols.Operator -> RuleDefs.getRuleOfOperator currentToken
    | Symbols.ConsistantOperator -> RuleDefs.getRuleOfConsistantOperator currentToken
    | Symbols.Assign -> RuleDefs.getRuleOfAssign currentToken
    | Symbols.Expr -> RuleDefs.getRuleOfExpr currentToken
    | Symbols.SubExpr -> RuleDefs.getRuleOfSubExpr currentToken
    | Symbols.SubExpr' -> RuleDefs.getRuleOfSubExpr' currentToken
    | Symbols.UnaryOp -> RuleDefs.getRuleOfUnaryOp currentToken
    | Symbols.BinaryOp -> RuleDefs.getRuleOfBinaryOp currentToken
    | Symbols.Operand -> RuleDefs.getRuleOfOperand currentToken
    | Symbols.ComplexOperator -> RuleDefs.getRuleOfComplexOperator currentToken


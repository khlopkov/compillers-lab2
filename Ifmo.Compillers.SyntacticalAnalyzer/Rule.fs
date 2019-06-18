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
    type RuleDef = Tokens.Token -> list<Rule>
    
    let private oneSymbolRule left right =
        { left = left; right = [right]}
        
    let private oneTokenRule = fun left ->
        Symbols.Terminal
        >> oneSymbolRule left
        
        
    let getRuleOfComplexOperator: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.If ->
                [{ left = Symbols.ComplexOperator; right = [
                    Symbols.Terminal Tokens.If;
                    Symbols.Terminal Tokens.OpenBracket;
                    Symbols.NonTerminal Symbols.Expr;
                    Symbols.Terminal Tokens.CloseBracket;
                    Symbols.NonTerminal Symbols.Operator;
                ]}]
            | _ -> list.Empty
        rulesByToken
 
        
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
        rulesByToken
        
    let getRuleOfOperand: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _
            | Tokens.Const _ ->
                [oneTokenRule Symbols.Operand t]
            | _ -> list.Empty
        rulesByToken
        
    let getRuleOfUnaryOp: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Minus 
            | Tokens.Not ->
                [oneTokenRule Symbols.UnaryOp t]
            | _ -> list.Empty
        rulesByToken
        
    let getRuleOfSubExpr: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.OpenBracket ->
                 [{ left = Symbols.SubExpr;
                    right = [ Symbols.Terminal Tokens.OpenBracket;
                    Symbols.NonTerminal Symbols.Expr;
                    Symbols.Terminal Tokens.CloseBracket;
                    Symbols.NonTerminal Symbols.SubExpr' ]
                 }]
            | Tokens.Id _
            | Tokens.Const _ ->
                [{ left = Symbols.SubExpr; right = [
                    Symbols.NonTerminal Symbols.Operand;
                    Symbols.NonTerminal Symbols.SubExpr';
                ] }]
                
            | _ -> list.Empty
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
                [{left = Symbols.SubExpr'; right = [
                    Symbols.NonTerminal Symbols.BinaryOp;
                    Symbols.NonTerminal Symbols.SubExpr;
                ]}]
            | _ -> [{left = Symbols.SubExpr'; right = []}]
        rulesByToken
        
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
        rulesByToken
     
    let getRuleOfAssign: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _ ->
                [ { left = Symbols.Assign;
                    right = [
                        Tokens.Id >> Symbols.Terminal <| "";
                        Symbols.Terminal Tokens.Assign;
                        Symbols.NonTerminal Symbols.Expr;
                        Symbols.Terminal Tokens.LineBreak ] } ]
            | _ -> list.Empty
        rulesByToken
                
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
        rulesByToken
        
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
        rulesByToken
    
    let getRuleOfOperatorList: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _
            | Tokens.If
            | Tokens.Begin ->
                [{left = Symbols.OperatorList; right = [
                    Symbols.NonTerminal Symbols.Operator;
                    Symbols.NonTerminal Symbols.OperatorList'
                ]}]
            | _ -> list.Empty
        rulesByToken
        
    let getRuleOfOperatorList': RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _
            | Tokens.If
            | Tokens.Begin ->
                [ Symbols.NonTerminal
                >> (oneSymbolRule <| Symbols.OperatorList')
                <| Symbols.OperatorList ]
            | _ -> [ epsilonRule Symbols.OperatorList' ]
        rulesByToken
        
    let getRuleOfVarList: RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _ -> 
                [{ left = Symbols.VarList; right = [
                    Symbols.Terminal t;
                    Symbols.NonTerminal Symbols.VarList';
                ] }]
            | _ -> list.Empty
        rulesByToken
        
    let getRuleOfVarList': RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.LineBreak _ -> 
                [{ left = Symbols.VarList'; right = [
                    Symbols.Terminal Tokens.LineBreak;
                    Symbols.NonTerminal Symbols.VarList''; ] }]
            | Tokens.Coma _ -> 
                [{ left = Symbols.VarList';
                    right = [
                    Symbols.Terminal Tokens.Coma;
                    Symbols.NonTerminal Symbols.VarList; ] }]
            | _ -> []
        rulesByToken
        
    let getRuleOfVarList'': RuleDef =
        let rulesByToken t =
            match t with
            | Tokens.Id _ -> 
                [{ left = Symbols.VarList''; right = [ Symbols.NonTerminal Symbols.VarList; ] }]
            | _ -> [epsilonRule Symbols.VarList'']
        rulesByToken
        
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
        rulesByToken
        
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
        rulesByToken
        
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


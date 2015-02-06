module Language.SAL.Syntax.Expression where


-- | SAL Expressions
data Expression = NameExpr
                | QualifiedNameExpr
                | NextVariable
                | Numeral
                | Application
                | InfixApplication
                | ArraySelection
                | RecordSelection
                | TupleSelection
                | UpdateExpression
                | LambdaAbstraction
                | QuantifiedExpression
                | LetExpression
                | SetExpression
                | ArrayLiteral
                | RecordLiteral
                | TupleLiteral
                | Conditional
                | GroupedExpression Expression
                | StatePred


type NameExpr = Name
type QualifiedNameExpr = QualifiedName
newtype NextVariable = NextVariable { var_name :: Identifier }  -- var'
data Application = Application Function Argument
data Function = Function Expression
type Argument = [Expression]  -- comma sep
data InfixApplication = InfixApplication Expression Identifier Expression
data ArraySelection = ArraySelection Expression Expression  -- Expression[Expression]
data RecordSelection = RecordSelection Expression Identifier  -- Expression.Identifier
data TupleSelection = TupleSelection Expression Numeral  -- Expression.Numeral
data UpdateExpression = UpdateExpression Expression Update  -- Expression WITH Update
data Update = Update UpdatePosition Expression  -- UpdatePosition := Expression
data UpdatePositionFrag = UPFA Argument
                        | UPFE Expression
                        | UPFI Identifier
                        | UPFN Numeral
data UpdatePosition = [UpdatePositionFrag]  -- {Argument | [Expression] | .Identifier | .Numeral}+
data LambdaAbstraction = LambdaAbstraction VarDecls Expression  -- LAMBDA (VarDecls) : Expression
type VarDecls = [VarDecl]  -- comma sep
data VarDecl = VarDecl [Identifier] Type  -- {Identifier}+, : Type
QuantifiedExpression = QuantifiedExpression Quantifier VarDecls Expression  -- Quantifier (VarDecls) : Expression

{-
Quantifier = FORALL | EXISTS
LetExpression = LET LetDeclarations IN Expression
LetDeclarations = {Identifier : Type = Expression}+,
SetExpression = SetListExpression | SetPredExpression
SetPredExpression = { Identifier : Type | Expression }
SetListExpression = { {Expression}+, }
ArrayLiteral = [[IndexVarDecl] Expression]
IndexVarDecl = Identifier : IndexType
RecordLiteral = (# {RecordEntry}+, #)
RecordEntry = Identifier := Expression
TupleLiteral = Argument
Conditional = IF Expression ThenRest
ThenRest = THEN Expression [ ElsIf ] ELSE Expression ENDIF
ElsIf = ELSIF Expression ThenRest
StatePred = Module . (INIT | TRANS)
-}

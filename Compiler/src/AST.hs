module AST where
data BinOp = Add
           | Subtract
           | Multiply
           | Divide
           | Lt
           | Gt
           | Eq
           | Leq
           | Geq
           | Neq
           | And
           | Or
             deriving (Show)

data Expression = Var String
                | IntConst Integer
                | StrConst String
                | BinaryOp BinOp Expression Expression -- TODO Not?
                | Call String [Expression]
                  deriving (Show)

data Statement = Sequence [Statement]
               | Assign String Expression
               | If Expression Statement Statement
               | While Expression Statement
               | Eval Expression
               | Return Expression
               | NoOp
                 deriving (Show)

data Definition = Function String [String] Statement -- TODO possibly add types to arguments
                | GlobalVar String Expression
                 deriving (Show)

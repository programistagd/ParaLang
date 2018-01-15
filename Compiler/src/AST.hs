module AST where
data BinOp = Add
           | Subtract
           | Multiply
           | Divide
             deriving (Show, Eq)

data Expression = Var String
                | Const Double
                | BinaryOp BinOp Expression Expression
                | Call String [Expression]
                  deriving (Show, Eq)

data VarValue = Expr Expression | Argument String

data Statement = Assign String Expression
               | Function String [String] Expression
               deriving (Show, Eq)

type Fun = (String, [String], Expression)
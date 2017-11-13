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
                | DotExpr Expression Expression -- TODO this is not the most beautiful way of handling structs etc.,
                                                -- because it syntactically allows for calls like 2.b() or c().(2+1) which is stupid,
                                                -- but it's very simple so for now I use it, later I might redo it
                | Call String [Expression]
                  deriving (Show)

data Statement = Sequence [Statement]
               | Assign Expression Expression
               | If Expression Statement Statement
               | While Expression Statement
               | Eval Expression
               | Return Expression
               | NoOp
                 deriving (Show)

data Definition = Function String [String] Statement -- TODO possibly add types to arguments
                | GlobalVar String Expression
                | Structure String String [Definition]
                 deriving (Show)

-- Structure inspired by https://wiki.haskell.org/Parsing_a_simple_imperative_language#Notes
-- Right now we go for a dynamic language, because implementing static checks is quite a lot of work
-- In some time I want to migrate to static typing, but now I want to quickly write a test lang.
module Parser where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

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

languageDef =
 emptyDef { Token.commentStart    = "/*"
          , Token.commentEnd      = "*/"
          , Token.commentLine     = "//"
          , Token.identStart      = letter
          , Token.identLetter     = alphaNum
          , Token.reservedNames   = [ "if"
                                    , "else"
                                    , "while"
                                    , "fun"
                                    , "true"
                                    , "false"
                                    ]
          , Token.reservedOpNames = ["+", "-", "*", "/", "=", "=="
                                    , "<", ">", "&&", "||", "<=", ">=", "!=", "!"
                                    ]
          }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
braces     = Token.braces     lexer -- parses surrounding braces
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
commaSep   = Token.commaSep   lexer -- parses zero or more lexemes separated by comma
stringLiteral = Token.stringLiteral lexer -- parses a string literal

functionCall :: Parser Expression
functionCall = do
    functionName <- identifier
    args <- parens (commaSep expression)
    return $ Call functionName args

operators = [ {-[Prefix (reservedOp "-"   >> return (Neg             ))],-}
              [Infix  (reservedOp "*"   >> return (BinaryOp Multiply)) AssocLeft,
               Infix  (reservedOp "/"   >> return (BinaryOp Divide  )) AssocLeft],
              [Infix  (reservedOp "+"   >> return (BinaryOp Add     )) AssocLeft,
               Infix  (reservedOp "-"   >> return (BinaryOp Subtract)) AssocLeft],
              [Infix  (reservedOp "<"   >> return (BinaryOp Lt      )) AssocLeft,
               Infix  (reservedOp ">"   >> return (BinaryOp Gt      )) AssocLeft,
               Infix  (reservedOp "=="   >> return (BinaryOp Eq     )) AssocLeft,
               Infix  (reservedOp "<="   >> return (BinaryOp Leq    )) AssocLeft,
               Infix  (reservedOp ">="   >> return (BinaryOp Geq    )) AssocLeft],
              [Infix  (reservedOp "||"   >> return (BinaryOp Or     )) AssocLeft,
               Infix  (reservedOp "&&"   >> return (BinaryOp And    )) AssocLeft]
            ]

term =  parens expression
    <|> try functionCall
    <|> fmap Var identifier
    <|> fmap IntConst integer
    <|> fmap StrConst stringLiteral

expression :: Parser Expression
expression = buildExpressionParser operators term

assignStmt :: Parser Statement
assignStmt =
  do var  <- identifier
     _    <- reservedOp "="
     expr <- expression
     return $ Assign var expr

ifStmt :: Parser Statement
ifStmt = do
    _    <- reserved "if"
    cond <- expression
    stmt <- statement
    return $ If cond stmt NoOp

ifElseStmt :: Parser Statement
ifElseStmt = do
    _     <- reserved "if"
    cond  <- expression
    stmt1 <- statement
    _     <- reserved "else"
    stmt2 <- statement
    return $ If cond stmt1 stmt2

whileStmt :: Parser Statement
whileStmt = do
    _    <- reserved "while"
    cond <- expression
    _    <- reserved "do"
    stmt <- statement
    return $ While cond stmt

returnStmt :: Parser Statement
returnStmt = do
    _     <- reserved "return"
    value <- expression
    return $ Return value

evalStmt :: Parser Statement
evalStmt = do
    expr <- expression
    return $ Eval expr

statement' :: Parser Statement
statement' =  try ifElseStmt
          <|> ifStmt
          <|> whileStmt
          <|> returnStmt
          <|> try assignStmt
          <|> evalStmt

sequenceOfStatements =
  do list <- many1 statement' --sepBy1 statement' semi
     return $ if length list == 1 then head list else Sequence list

statement :: Parser Statement
statement =  braces statement
         <|> sequenceOfStatements

function :: Parser Definition
function = do
    _            <- reserved "fun"
    functionName <- identifier
    args <- parens (commaSep identifier)
    body <- statement
    return $ Function functionName args body

globalVariable :: Parser Definition
globalVariable = do
    _ <- reserved "var"
    name <- identifier
    _ <- reservedOp "="
    value <- expression
    return $ GlobalVar name value

definition :: Parser Definition
definition =  function
          <|> globalVariable

parseProgram :: String -> String -> [Definition]
parseProgram code fname =
  case parse ((whiteSpace >> many definition) <* eof) fname code of
    Left e -> error $ show e
    Right r -> r

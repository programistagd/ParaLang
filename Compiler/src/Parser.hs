-- Structure inspired by https://wiki.haskell.org/Parsing_a_simple_imperative_language#Notes
-- Right now we go for a dynamic language, because implementing static checks is quite a lot of work
-- In some time I want to migrate to static typing, but now I want to quickly write a test lang.
module Parser where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import AST

languageDef =
 emptyDef { Token.commentStart    = "/*"
          , Token.commentEnd      = "*/"
          , Token.commentLine     = "//"
          , Token.identStart      = letter
          , Token.identLetter     = alphaNum
          , Token.reservedNames   = [ "if"
                                    , "else"
                                    , "while"
                                    , "func"
                                    , "true"
                                    , "false"
                                    , "struct"
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
dot = Token.dot lexer -- parses a dot
brackets = Token.brackets lexer -- parses brackets

functionCall :: Parser Expression
functionCall = do
    functionName <- identifier
    args <- parens (commaSep expression)
    return $ Call functionName args

--TODO For now we can only access top level arrays, because of parser limitations, later I might do something about it
arrayAccess :: Parser Expression
arrayAccess = do
    arrayName <- identifier
    index <- brackets expression
    return $ AccessArray (Var arrayName) index

operators = [ {-[Prefix (reservedOp "-"   >> return (Neg             ))],-}
              [Infix  (dot              >> return DotExpr) AssocLeft          ],
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
    <|> try arrayAccess
    <|> try functionCall
    <|> fmap Var identifier
    <|> fmap IntConst integer
    <|> fmap StrConst stringLiteral

expression :: Parser Expression
expression = buildExpressionParser operators term

assignStmt :: Parser Statement
assignStmt =
  do ref  <- expression
     _    <- reservedOp "="
     expr <- expression
     return $ Assign ref expr

ifStmt :: Parser Statement
ifStmt = do
    _    <- reserved "if"
    cond <- parens expression
    stmt <- statement
    return $ If cond stmt NoOp

ifElseStmt :: Parser Statement
ifElseStmt = do
    _     <- reserved "if"
    cond  <- parens expression
    stmt1 <- statement
    _     <- reserved "else"
    stmt2 <- statement
    return $ If cond stmt1 stmt2

whileStmt :: Parser Statement
whileStmt = do
    _    <- reserved "while"
    cond <- parens expression
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
statement =  statement'
         <|> braces sequenceOfStatements

function :: Parser Definition
function = do
    _            <- reserved "func"
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

structure :: Parser Definition
structure = do
    _ <- reserved "struct"
    name <- identifier
    typename <- option "" identifier
    defs <- braces $ many definition
    return $ Structure name typename defs

definition :: Parser Definition
definition =  function
          <|> globalVariable
          <|> structure

parseProgram :: String -> String -> [Definition]
parseProgram code fname =
  case parse ((whiteSpace >> many definition) <* eof) fname code of
    Left e -> error $ show e
    Right r -> r

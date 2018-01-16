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
          , Token.reservedNames   = ["func"]
          , Token.reservedOpNames = ["+", "-", "*", "/", "^", "="]
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
--integer    = Token.integer    lexer -- parses an integer
naturalOrFloat = Token.naturalOrFloat      lexer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
commaSep   = Token.commaSep   lexer -- parses zero or more lexemes separated by comma
-- stringLiteral = Token.stringLiteral lexer -- parses a string literal
-- dot = Token.dot lexer -- parses a dot
-- brackets = Token.brackets lexer -- parses brackets

functionCall :: Parser Expression
functionCall = do
    functionName <- identifier
    args <- parens (commaSep expression)
    return $ Call functionName args

operators = [ {-[Prefix (reservedOp "-"   >> return (Neg             ))],-}
              [Infix  (reservedOp "^"   >> return (BinaryOp Exp)) AssocLeft],
              [Infix  (reservedOp "*"   >> return (BinaryOp Multiply)) AssocLeft,
               Infix  (reservedOp "/"   >> return (BinaryOp Divide  )) AssocLeft],
              [Infix  (reservedOp "+"   >> return (BinaryOp Add     )) AssocLeft,
               Infix  (reservedOp "-"   >> return (BinaryOp Subtract)) AssocLeft]
            ]

constant :: Either Integer Double -> Expression
constant e = case e of
    Left x -> Const (fromIntegral x)
    Right x -> Const x

term =  parens expression
    <|> try functionCall
    <|> fmap Var identifier
    <|> fmap constant naturalOrFloat

expression :: Parser Expression
expression = buildExpressionParser operators term

assignStmt :: Parser Statement
assignStmt = do 
    ref  <- identifier
    _    <- reservedOp "="
    expr <- expression
    _ <- optional semi
    return $ Assign ref expr

funcStmt :: Parser Statement
funcStmt = do
    _     <- reserved "func"
    name <- identifier
    args <- parens (commaSep identifier)
    _     <- reservedOp "="
    body <- expression
    _ <- optional semi
    return $ Function name args body

statement :: Parser Statement
statement =  try assignStmt
          <|> funcStmt

parseProgram :: String -> String -> [Statement]
parseProgram code fname =
  case parse ((whiteSpace >> many statement) <* eof) fname code of
    Left e -> error $ show e
    Right r -> r

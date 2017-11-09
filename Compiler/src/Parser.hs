-- Structure inspired by https://wiki.haskell.org/Parsing_a_simple_imperative_language#Notes

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
               deriving (Show)

data Expression = Var String
                | IntConst Integer
                | BinaryOp BinOp Expression Expression
                  deriving (Show)

data Statement = Sequence [Statement]
               | Assign String Expression
               | If Expression Statement Statement
               | While Expression Statement
               | NoOp
                 deriving (Show)

languageDef =
 emptyDef { Token.commentStart    = "/*"
          , Token.commentEnd      = "*/"
          , Token.commentLine     = "//"
          , Token.identStart      = letter
          , Token.identLetter     = alphaNum
          , Token.reservedNames   = [ "if"
                                    , "then"
                                    , "else"
                                    , "while"
                                    , "do"
                                    , "skip"
                                    , "true"
                                    , "false"
                                    , "not"
                                    , "and"
                                    , "or"
                                    ]
          , Token.reservedOpNames = ["+", "-", "*", "/", "=", "=="
                                    , "<", ">", "&&", "||", "!"
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

operators = [ {-[Prefix (reservedOp "-"   >> return (Neg             ))],-}
              [Infix  (reservedOp "*"   >> return (BinaryOp Multiply)) AssocLeft,
               Infix  (reservedOp "/"   >> return (BinaryOp Divide  )) AssocLeft],
              [Infix  (reservedOp "+"   >> return (BinaryOp Add     )) AssocLeft,
               Infix  (reservedOp "-"   >> return (BinaryOp Subtract)) AssocLeft]
            ]

term =  parens expression
    <|> fmap Var identifier
    <|> fmap IntConst integer

expression :: Parser Expression
expression = buildExpressionParser operators term

assignStmt :: Parser Statement
assignStmt =
  do var  <- identifier
     void $ reservedOp "="
     expr <- expression
     return $ Assign var expr

statement' :: Parser Statement
statement' =   {-ifStmt
          <|> whileStmt
          <|> skipStmt
          <|> -}assignStmt

sequenceOfStatements =
  do list <- many1 statement' --sepBy1 statement' semi
     return $ if length list == 1 then head list else Sequence list

statement :: Parser Statement
statement =  braces statement
         <|> sequenceOfStatements

data Definition = Whatever
                  deriving (Show)

--parseProgram :: String -> [Definition]
--parseProgram code =
--  [Whatever]

parseProgram :: String -> String -> Statement
parseProgram code fname =
  case parse ((whiteSpace >> statement) <* eof) fname code of
    Left e -> error $ show e
    Right r -> r

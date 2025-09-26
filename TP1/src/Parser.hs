module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = try (term `chainl1` plusExp) 
         <|> 
         try (term `chainl1` minusExp) 
         <|>
         term
  where
  plusExp :: Parser (Exp Int -> Exp Int -> Exp Int)
  plusExp = do { reservedOp lis "+";  return Plus }

  minusExp :: Parser (Exp Int -> Exp Int -> Exp Int)
  minusExp = do { reservedOp lis "-";  return (Minus) }


term :: Parser (Exp Int)
term = try (unary `chainl1` timesExp) 
       <|> 
       try (unary `chainl1` divExp) 
       <|>
       unary  
  where
  timesExp :: Parser (Exp Int -> Exp Int -> Exp Int)
  timesExp = do { reservedOp lis "*";  return Times }

  divExp :: Parser (Exp Int -> Exp Int -> Exp Int)
  divExp = do { reservedOp lis "/";  return Div }

unary :: Parser (Exp Int)
unary = try (do { reservedOp lis "-"; a <- atom; return (UMinus a) }) <|> atom
  where
  uMinusExp :: Parser (Exp Int -> Exp Int)
  uMinusExp = do { reservedOp lis "-";  return UMinus }

atom :: Parser (Exp Int)
atom = try nat <|> try (varExp) <|> (parens lis intexp)
  where
  nat :: Parser (Exp Int)
  nat = do {
            n <- natural lis;
            return (Const (fromIntegral n))
           }
  varExp :: Parser (Exp Int)
  varExp = do {
                v <- identifier lis;
                try (do { reservedOp lis "+";
                          reservedOp lis "+";
                          return (VarInc v) 
                        }) 
                <|>
                return (Var v)
              }


------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = undefined

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = undefined


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

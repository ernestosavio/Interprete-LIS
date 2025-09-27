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
intExp :: Parser (Exp Int)
intExp = try (intTerm `chainl1` intExpOp) 
         <|>
         intTerm
  where
  plusExp :: Parser (Exp Int -> Exp Int -> Exp Int)
  plusExp = do { reservedOp lis "+"; return Plus }

  minusExp :: Parser (Exp Int -> Exp Int -> Exp Int)
  minusExp = do { reservedOp lis "-"; return Minus }

  intExpOp :: Parser (Exp Int -> Exp Int -> Exp Int)
  intExpOp = try minusExp <|> plusExp 


intTerm :: Parser (Exp Int)
intTerm = try (intUnary `chainl1` termExpOp) 
          <|>
          intUnary  
  where
  timesExp :: Parser (Exp Int -> Exp Int -> Exp Int)
  timesExp = do { reservedOp lis "*"; return Times }

  divExp :: Parser (Exp Int -> Exp Int -> Exp Int)
  divExp = do { reservedOp lis "/"; return Div }

  termExpOp :: Parser (Exp Int -> Exp Int -> Exp Int)
  termExpOp = try timesExp <|> divExp 

intUnary :: Parser (Exp Int)
intUnary = try (do { reservedOp lis "-"; u <- intUnary; return (UMinus u) }) <|> intAtom


intAtom :: Parser (Exp Int)
intAtom = try nat <|> try (varExp) <|> (parens lis intExp)
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

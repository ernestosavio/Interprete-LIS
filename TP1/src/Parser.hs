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

boolExp :: Parser (Exp Bool)
boolExp = try (boolTerm `chainl1` boolExpOp) 
         <|>
         boolTerm
  where
  boolExpOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
  boolExpOp = do { reservedOp lis "||"; return Or }

boolTerm :: Parser (Exp Bool)
boolTerm = try (boolUnary `chainl1` boolTermOp) 
         <|>
         boolUnary
  where
  boolTermOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
  boolTermOp = do { reservedOp lis "&&"; return And }

boolUnary :: Parser (Exp Bool)
boolUnary = try (do { reservedOp lis "!"; u <- boolUnary; return (Not u) }) <|> boolAtom

boolAtom :: Parser (Exp Bool)
boolAtom = try boolean <|> (parens lis boolExp) <|> try (boolComp)
  where
  boolean :: Parser (Exp Bool)
  boolean = try (do { reserved lis "true"; return BTrue})
            <|>
            (do { reserved lis "false"; return BFalse})

boolComp :: Parser (Exp Bool)
boolComp = do { x <- intExp;
                choice [try (do {reservedOp lis "=="; y <- intExp; return (Eq x y)}),
                        try (do {reservedOp lis "!="; y <- intExp; return (NEq x y)}),
                        try (do {reservedOp lis "<"; y <- intExp; return (Gt x y)}),
                        try (do {reservedOp lis ">"; y <- intExp; return (Lt x y)})]
              }
  
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

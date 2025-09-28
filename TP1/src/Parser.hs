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
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until", "case"]
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
intExp = intTerm `chainl1` intExpOp
        
  where
  plusExp :: Parser (Exp Int -> Exp Int -> Exp Int)
  plusExp = do { reservedOp lis "+"; return Plus }

  minusExp :: Parser (Exp Int -> Exp Int -> Exp Int)
  minusExp = do { reservedOp lis "-"; return Minus }

  intExpOp :: Parser (Exp Int -> Exp Int -> Exp Int)
  intExpOp = try minusExp <|> plusExp 


intTerm :: Parser (Exp Int)
intTerm = intUnary `chainl1` termExpOp
  
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
boolExp = boolTerm `chainl1` boolExpOp
  where
  boolExpOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
  boolExpOp = do { reservedOp lis "||"; return Or }

boolTerm :: Parser (Exp Bool)
boolTerm = boolUnary `chainl1` boolTermOp
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
                        try (do {reservedOp lis "<"; y <- intExp; return (Lt x y)}),
                        try (do {reservedOp lis ">"; y <- intExp; return (Gt x y)})]
              }
  
-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = commAtom `chainl1` commOp

  where
  commOp :: Parser (Comm -> Comm -> Comm)
  commOp = do { reservedOp lis ";"; return Seq }

commAtom :: Parser Comm
commAtom = try commAss <|> try commRepeat <|> try commIf <|> try commCase <|> commSkip
  where
  commAss :: Parser Comm
  commAss = do { v <- identifier lis; 
                 reservedOp lis "="; 
                 e <- intExp; 
                 return (Let v e)
              }

  commRepeat :: Parser Comm
  commRepeat = do { reserved lis "repeat"; 
                    symbol lis "{"; 
                    c <- comm; 
                    symbol lis "}"; 
                    reserved lis "until"; 
                    b <- boolExp; 
                    return (RepeatUntil c b) 
                }
  
  commSkip :: Parser Comm
  commSkip = do { reserved lis "skip"; 
                  return Skip
              }

  commElse :: (Exp Bool) -> Comm -> Parser Comm
  commElse b c1 = do { reserved lis "else"; 
                       symbol lis "{"; 
                       c2 <- comm; 
                       symbol lis "}"; 
                       return (IfThenElse b c1 c2)
                    }

  commIf :: Parser Comm
  commIf = do { reserved lis "if";
                b <- boolExp;
                symbol lis "{";
                c <- comm;
                symbol lis "}";
                try (commElse b c) <|> return (IfThen b c)
              }

  caseComm :: Parser [(Exp Bool,Comm)]
  caseComm = do { b <- boolExp;
                   symbol lis ":";
                   symbol lis "{";
                   c <- comm;
                   symbol lis "}";
                   xc <- caseComm;
                   return ((b,c):xc)
                } <|> return []

  commCase :: Parser Comm
  commCase = do { reserved lis "case";
                  symbol lis "{";
                  xc <- caseComm;
                  symbol lis "}";
                  return (Case xc)
                }

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

-- {-# LANGUAGE RankNTypes #-}
module Text.ExprParser.ParseRownum (ExprParam(..), expr, parserRNS, runRNT, runRNS) 
    where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String(Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Data.Text(Text, unpack, pack)
import Data.Functor.Identity(Identity())

data ExprParam = ExprParam {
        epRownum :: Int,
        epCount :: Int
    }
         
expr :: ExprParam -> Parser Int
expr n   = buildExpressionParser table (term n) <?> "expression"

lexer :: P.GenTokenParser String u Identity
lexer       = P.makeTokenParser haskellDef 

natural :: Parsec String u Integer
--natural = fmap read $ many1 digit
natural = P.natural lexer
-- look at http://stackoverflow.com/questions/11097153

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = P.parens lexer

braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces = P.braces lexer

reservedOp :: String -> ParsecT String u Identity ()
reservedOp = P.reservedOp lexer

term :: ExprParam -> ParsecT String () Identity Int
term n  =  parens (expr n)
        <|> braces (expr n)
        <|> (spaces >> string "if"   >> spaces >> expr n >>= \e1 -> 
             spaces >> string "then" >> spaces >> expr n >>= \e2 -> 
             spaces >> string "else" >> spaces >> expr n >>= \e3 -> 
             return (if e1 /= 0 then e2 else e3))
        <|> (spaces >> fmap fromInteger natural >>= \r -> spaces >> return r)
        <|> (spaces >> string "rownum" >> spaces >> return (epRownum n))
        <|> (spaces >> string "count" >> spaces >> return (epCount n))
        <?> "simple expression"
        
table :: [[Operator String u Identity Int]]        
table   = [ [prefix "-" negate, prefix "+" id ]
          , [postfix "++" (+1)] -- "--" is defined as comments in haskellDef so we can't have such operation...
          , [binary "mod" mod AssocLeft]
          , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
          , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
          , [
                  binary "=="       (ifn (==))  AssocLeft
                , binary "!="       (ifn (/=))  AssocLeft
                , binary "/="       (ifn (/=))  AssocLeft
                , binary ">"        (ifn (>))   AssocLeft
                , binary "&gt;"     (ifn (>))   AssocLeft
                , binary "<"        (ifn (<))   AssocLeft
                , binary "&lt;"     (ifn (<))   AssocLeft
                , binary ">="       (ifn (>=))  AssocLeft
                , binary "&gt;="    (ifn (>=))  AssocLeft
                , binary "<="       (ifn (<=))  AssocLeft
                , binary "&lt;="    (ifn (<=))  AssocLeft
            ]
          ]
    where
        ifn f a b = if f a b then 1 else 0

        
binary :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
binary  name fun assoc = Infix (do{ spaces; reservedOp name; spaces; return fun }) assoc

prefix :: String -> (a -> a) -> Operator String u Identity a
prefix  name fun       = Prefix (do{ reservedOp name; return fun })

postfix :: String -> (a -> a) -> Operator String u Identity a
postfix name fun       = Postfix (do{ reservedOp name; return fun })

parserRNS :: ExprParam -> Parser String
parserRNS n = many (noneOf "{") >>= \s1 -> 
            try ((
                        try (char '{' >> expr n >>= \r -> char '}' >> return (s1 ++ show r)) 
                    <|> fmap ((s1++).(:[])) anyChar
                ) >>= \s2 -> fmap (s2++) (parserRNS n))
        <|> (eof >> return s1)

runRNS :: ExprParam -> String -> String
runRNS n = either (error . show) id . runParser (parserRNS n) () "Rownum parser"

runRNT :: ExprParam -> Text -> Text
runRNT n = pack . runRNS n . unpack

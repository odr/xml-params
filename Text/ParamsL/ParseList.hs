{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}
module Text.ParamsL.ParseList where

import Control.Applicative((<$>),(<*>), pure)
import Control.Arrow(first, second, (***))
import Control.Monad(liftM2, filterM)
import Data.Char(isSpace)
import Data.Function(on)
import Data.List -- (foldl', (\\), intersectBy)
import Data.Maybe(listToMaybe)
import Data.Monoid(Monoid(..))
import Data.Ord(comparing)
import qualified Data.Text as T
import Data.XML.Types
-- import Text.Multiline(ml)
import Text.Parsec

import Text.ExprParser.ParseRownum(runRNT, ExprParam(..))

data ParamsL a b = ParamsL { pNames :: [a], pVals :: [[b]] }
type ParamsL' = ParamsL T.Text T.Text

pZero :: ParamsL a b
pZero = ParamsL [] []

paramsToList :: ParamsL a b -> [ParamsL a b]
paramsToList (ParamsL ns vss) = map (ParamsL ns . (:[])) vss

listToParams :: [ParamsL a b] -> ParamsL a b
listToParams [] = ParamsL [] [[]]
listToParams ps = ParamsL (pNames (head ps)) $ concatMap pVals ps


deriving instance (Show a, Show b) => Show (ParamsL a b)
deriving instance (Eq a, Eq b) => Eq (ParamsL a b)

instance (Eq a, Ord a, Eq b) => Monoid (ParamsL a b) where
    mempty = ParamsL mempty [mempty]
    mappend p1@(ParamsL ns1 vss1) p2@(ParamsL ns2 vss2)
        | p1 == mempty                      = p2
        | p2 == mempty                      = p1
        | null vss1 && (not $ null ns1) ||
          null vss2 && (not $ null ns2)     = pZero
        | null vss' && null ns'             = mempty        -- из-за того, что transpose [[]] = []
        | otherwise                         = ParamsL ns' vss'
        where
            ns = ns1 ++ ns2
            vss = liftM2 (++) vss1 vss2
            (ns', vss') = 
                    second (map (map head) . filter (all (null.tail)) . transpose) 
                    $ unzip 
                    $ map ((head *** (map nub . transpose)) . unzip) 
                    $ groupBy ((==) `on` fst) $ sortBy (comparing fst) 
                    $ zip ns 
                    $ transpose vss
    
(<+>) :: ParamsL' -> ParamsL' -> ParamsL'
p1 <+> p2 = ParamsL ns' vss'
    where
        pair (ParamsL ns vss) = zip ns $ transpose vss
        (ns', vss') = 
                second transpose $ unzip $ map ((head *** concat) . unzip) 
                $ groupBy ((==) `on` fst) $ sortBy (comparing fst) $ pair p1 ++ pair p2

reverseParams :: ParamsL a b -> ParamsL a b
reverseParams (ParamsL ns vss) = ParamsL ns $ reverse vss

projectionParam :: (Eq a, Eq b) => [a] -> ParamsL a b -> ParamsL a b
projectionParam ns0 (ParamsL ns vss) 
    | ns0 == mempty = ParamsL [] [[]]
    | otherwise = uncurry ParamsL $ second (nub . transpose) $ unzip $ filter ((`elem` ns0) . fst) $ zip ns $ transpose vss

(*->) :: (Eq a, Ord a, Eq b) => ParamsL a b -> ParamsL a b -> ParamsL a b
p1 *-> p2 = projectionParam (pNames p2) $ p1 `mappend` p2

type ParserL = Parsec String Int 

parseL :: ParserL ParamsL'
parseL = fmap (\res -> if null res then mempty else foldr1 (<+>) res) sumList -- парсится сумма произведений (например, A*B+A*D)
    where
        sumList = parseProd `sepBy` char '+'
        parseProd = fmap mconcat $ parseL1 `sepBy` oneOf ";*"
        readLex = readTuple term
        parseL1 = readLex >>= \ns -> char '?' >> readTuple readLex >>= \vs -> return (ParamsL (map trim ns) $ map (map trim . take (length ns)) vs)
        trim = T.pack . trim' . trim'
            where trim' = reverse . dropWhile isSpace

readTuple :: ParserL a -> ParserL [a]
readTuple p  = noSpaces $ char '[' >> (noSpaces p `sepBy` char ',') >>= \r -> char ']' >> return r --(map trim r)
    where
        noSpaces x = spaces >> x >>= \r -> spaces >> return r
        
term :: ParserL String
term = fmap concat $ many1 termSimple
    where
        term0 = {- fmap trim $ -} many1 $ noneOf ",]{"
        term1 = braces -- char '{' >> many (noneOf "}") >>= \res -> char '}' >> return ('{' : res ++ "}")
        term2 = char '`' >> many (noneOf "`") >>= \res -> char '`' >> return res
        termSimple = choice [term2, term1, term0]
        
braces :: ParserL String
braces = char '{' >> fmap ('{':) go
    where
        go = do 
            s <- getState 
            if s < 0 
                then putState 0 >> return ""
                else do
                    res <- many (noneOf "{}")
                    c <- choice [char '}', char '{']
                    modifyState (if c == '{' then (+1) else (subtract 1))
                    fmap ((res++[c])++) go
       
getParamsL :: String -> ParamsL'
getParamsL s = p { pVals = concatMap (\p' -> concatMap (applyParam p') $ pVals p') $ paramsToList p }
    where
        p = case runParser parseL 0 "Params List Parser" s of
            Left err -> error $ unlines ["Error in getParamsL. String to parse: ", s, "", "Error: ", show err]
            Right xs -> xs
    
applyParam :: ApplyParam t => ParamsL' -> t -> [t]
applyParam (ParamsL ns vss) t = 
    zipWith (\k vs -> appP t (ExprParam {epRownum = k, epCount = cnt}) (defBound, (ns,vs))) [1..] vss
    where cnt = length vss
    
fstParam :: (Eq t, ApplyParam t, Monad m, Functor m) => (t -> m Bool) -> ParamsL' -> t -> m (Maybe ParamsL')
fstParam f (ParamsL ns vss) t 
    = fmap listToMaybe 
    $ fmap (map snd) 
    $ filterM (f . fst) 
    $ zipWith   (\k vs -> 
                ( appP t ExprParam {epRownum = k, epCount = cnt} (defBound, (ns,vs))
                , ParamsL ns [vs] )) 
            [1..] vss
    where cnt = length vss

class ApplyParam t where
    -- | применение одного кортежа параметров
    appP :: 
        t                                           -- | значение, к которому применяется параметр
        -> ExprParam                                -- | дополнительные параметры - текущий номер параметра и общее количество параметров (obsolete?)
        -> (T.Text->T.Text, ([T.Text], [T.Text]))   -- | (функция, помещающая название параметра в скобки; пара названия - значения)
        -> t
    
defBound :: T.Text -> T.Text
defBound n = mconcat ["{",n,"}"]    
    
instance ApplyParam T.Text where
    appP t n (fb, (ns,vs)) = runRNT n $ fst $ head $ dropWhile (uncurry (/=)) $ zip zs (tail zs)
        where
            zs = iterate (\z -> foldl' rep z $ zip ns vs) t
            rep tt (n',v') = T.replace (fb n') v' tt
    
instance (ApplyParam a) => ApplyParam (Maybe a) where
    appP t n p = fmap (\t' -> appP t' n p) t

instance (ApplyParam a) => ApplyParam [a] where
    appP t n p = fmap (\t' -> appP t' n p) t

instance (ApplyParam a, ApplyParam b) => ApplyParam (a, b) where
    appP (a,b) n p = (appP a n p, appP b n p)

instance ApplyParam Name where
    appP (Name n nsp px) k           = (Name <$> appP n k <*> appP nsp k <*> appP px k) . first (const $ \t -> mconcat ["p_",t,"_p"])
    
instance ApplyParam Content where
    appP (ContentText t) k           = ContentText       <$> appP t k
    appP (ContentEntity t) k         = ContentEntity     <$> appP t k
    
instance ApplyParam ExternalID where
    appP (SystemID t) k              = SystemID          <$> appP t k
    appP (PublicID t1 t2) k          = PublicID          <$> appP t1 k <*> appP t2 k

instance ApplyParam Instruction	where
    appP (Instruction t d) k         = Instruction       <$> appP t k <*> appP d k

instance ApplyParam Event where
    appP EventBeginDocument	       _ = pure EventBeginDocument
    appP EventEndDocument          _ = pure EventEndDocument
    appP (EventBeginDoctype t mid) k = EventBeginDoctype <$> appP t k <*> appP mid k
    appP EventEndDoctype           _ = pure EventEndDoctype
    appP (EventInstruction i)      k = EventInstruction  <$> appP i k
    appP (EventBeginElement n xs)  k = EventBeginElement <$> appP n k <*> appP xs  k
    appP (EventEndElement n)       k = EventEndElement   <$> appP n k
    appP (EventContent c)	       k = EventContent      <$> appP c k
    appP (EventComment t)          k = EventComment      <$> appP t k
    appP (EventCDATA t)            k = EventCDATA        <$> appP t k


applyString :: String -> T.Text -> [T.Text]
applyString s = applyParam (getParamsL s)

{-# LANGUAGE OverloadedStrings #-}
module Text.ParamsL.ProcessParams where

import Control.Monad(when)
import Data.Conduit(Conduit, await, leftover, yield)
import Data.Default(Default(..))
import Data.List(foldl', transpose)
import Data.Maybe(fromMaybe)
import Data.XML.Types(Event(..), Content(..), Name(..))
-- import Debug.Trace

import Text.ParamsL.ParseList(ParamsL', applyParam, getParamsL, reverseParams, paramsToList)

-- | State of transformation
data ParamsState = ParamsState {
        globalParams    :: [ParamsL'],
        -- ^ params outside of root element
        localParams     :: [(ParamsL', [(ParamsL', [Event])])],
        -- ^ params inside element with list of events for these params
        inEval          :: Bool
        -- ^ if we are inside root element or not
    } deriving Show

-- | Environment of transformation
data ParamsEnv = ParamsEnv 
    { peIsParamsLElem :: Name -> Bool -- ^ define special element
    , peIsParamsLAttr :: Name -> Bool -- ^ define special attribute
    , peIsRoot        :: Name -> Bool -- ^ define root element
    }

instance Default ParamsEnv where
    def = ParamsEnv 
        { peIsParamsLElem   = (=="paramsL") . nameLocalName
        , peIsParamsLAttr   = (=="paramsL") . nameLocalName
        , peIsRoot          = (=="eval") . nameLocalName
        }

processParams :: (Monad m) => ParamsEnv -> ParamsState -> Conduit Event m [Event]
processParams pe0 ps0 = 
        go (ps0, 0::Int)
    where
        go (ps,cnt) = await >>= \mx -> case mx of
            Just (EventBeginElement n ats)  -> goBE n ats
            Just e@(EventEndElement n)      -> goEE e n
            Just e                          -> goEvent e
            Nothing                         -> goEnd
            where
                goBE n ats
                    | rootElem n    = go (ps    { localParams = [   ( mconcat (getParams : globalParams ps)
                                                                    , [(mempty, cleanBE)]
                                                                    ) ]
                                                , inEval = True 
                                                }, cnt)
                    | inEval ps     = go (ps    { localParams = (getParams, [(mempty,cleanBE)]) : localParams ps 
                                                }, cnt)
                    | otherwise     = yield cleanBE >> go (ps   { globalParams = getParams : globalParams ps 
                                                                }, cnt+1)
                    where
                        getParams :: ParamsL'
                        getParams = reverseParams $ fromMaybe mempty $ fmap parseParams $ lookup "paramsL" ats
                            where
                                parseParams = getParamsL . foldl' (\acc c -> acc `mappend` showContent c) ""
                                showContent (ContentText t) = t
                                showContent _ = ""

                        notParamName = not . peIsParamsLAttr pe0 . fst -- (/="paramsL").nameLocalName.fst
                        cleanBE
                            | isParamsL n   = []
                            | otherwise     = [EventBeginElement n $ filter notParamName ats]
                goEE e n
                    | rootElem n    = case localParams ps of
                        ((p,pes):_)     -> let ps' = ps {localParams = [], inEval = False} in do
                                mapM_ yield   
                                        $ reverse $ map (reverse . concat) 
                                        $ transpose $ map (map snd) 
                                        $ calcParam p pes
                                go (ps', cnt) 
                        _               -> fail "Error in processParams (EventEndElement). LocalParams are empty on the end of eval."
                    | inEval ps     = go (ps    { localParams = reduceParams $ localParams ps 
                                                }, cnt) 
                    | cnt == 0      = leftover e
                    | otherwise     = case globalParams ps of
                        (_:pps)         -> do
                            when (not $ isParamsL n) $ yield [e]
                            go (ps { globalParams = pps }, cnt-1) 
                        _               -> fail "Error in processParams (EventEndElement). GlobalParams are empty but counter is more than 0."
                    where
                        calcParam :: ParamsL' -> [(ParamsL', [Event])] -> [[(ParamsL', [Event])]]
                        calcParam _ [] 
                            | null $ cleanEE = []
                            | otherwise = [[(mempty, cleanEE)]]
                        calcParam p pes1@((pe1,es1):pes) = map (\(pe, es) -> 
                                map (\p' -> let pe' = p' `mappend` pe in (pe', concat $ applyParam pe' es)) 
                                $ paramsToList p
                            ) pes2
                            where
                                pes2
                                    | pe1 == mempty  = (pe1, cleanEE ++ es1) : pes
                                    | null cleanEE  = pes1
                                    | otherwise     = (mempty, cleanEE) : pes1
                                    
                        cleanEE
                            | isParamsL n   = []
                            | otherwise     = [EventEndElement n]
                        reduceParams ((p, pes):(p2, pes2):lps) = (p2, pesIn ++ pes2) : lps
                            where 
                                pesIn = concat $ transpose $ calcParam p pes
                        reduceParams _  = error "Error in processParams (EventEndElement). LocalParams are empty inside eval."
                
                goEvent e
                    | inEval ps = case localParams ps of
                        (p,[]) : lps            -> go (ps   { localParams = (p, [(mempty, [e])]) : lps
                                                            }, cnt)
                        (p,pes0@((pe,es):pes)) : lps 
                            | pe == mempty      -> go (ps   { localParams = (p, (pe,e:es):pes) : lps
                                                            }, cnt) 
                            | otherwise         -> go (ps   { localParams = (p, (mempty,[e]):pes0) : lps
                                                            }, cnt)
                        _                       -> fail "Error in processParams (some Event). LocalParams are empty inside eval."
                    | otherwise = yield [e] >> go (ps,cnt)
            
                goEnd
                    | inEval ps = fail "Error in processParams. It was closed inside eval. Unbalanced xml-tree"
                    | otherwise = return ()

        isParamsL = peIsParamsLElem pe0 
                -- (`elem` ["{http://lukoil.ru/monitoring}paramsL", "{http://lukoil.ru/scheduler}paramsL"])
        rootElem = peIsRoot pe0 
                -- (`elem` ["{http://lukoil.ru/scheduler}eval", "{http://lukoil.ru/scheduler}exec", "{http://lukoil.ru/monitoring}eval"])

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- import qualified Data.ByteString as BS
import Control.Monad.Trans.Resource(runResourceT)
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.String
import Data.XML.Types
import System.Environment(getArgs)
import qualified Text.XML.Stream.Parse as XP
import qualified Text.XML.Stream.Render as XR
import Data.Default(def)

import Text.ParamsL.ProcessParams(processParams, ParamsState(..), ParamsEnv(..))

main :: IO ()
main = getArgs >>= doArgs
    where
        doArgs args = case args of
            f:f2:_ -> parseFileRules f f2
            _ -> error "Usage: antiParamsL <sourceFile> <resultFile>"
                
parseFileRules :: String -> String -> IO ()
parseFileRules fn f2 = runResourceT $ 
        XP.parseFile XP.def (fromString fn) 
        $$ processParams def (ParamsState [] [] False) 
        =$= CL.concatMap id 
        =$= XR.prettify 
        =$= XR.renderBytes XR.def 
        =$= CB.sinkFile (fromString f2)
    


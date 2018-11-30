{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- import qualified Data.ByteString as BS
import           Conduit
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.Conduit.List as CL
import           Data.Default (def)
import           Data.String
import           System.Environment (getArgs)
import qualified System.IO as IO
import           Text.ParamsL.ProcessParams (ParamsState(..), processParams)
import qualified Text.XML.Stream.Parse as XP
import qualified Text.XML.Stream.Render as XR


main :: IO ()
main = getArgs >>= doArgs
  where
    doArgs args = case args of
      f:f2:_ -> parseFileRules f f2
      _      -> error "Usage: antiParamsL <sourceFile> <resultFile>"

parseFileRules :: String -> String -> IO ()
parseFileRules fn f2 = runResourceT $ runConduit
  $ XP.parseFile XP.def (fromString fn)
  .| processParams def (ParamsState [] [] False)
  .| CL.concatMap id
  .| CL.map Chunk
  .| XR.prettify
  .| XR.renderBuilderFlush XR.def
  .| builderToByteStringWithFlush (allNewBuffersStrategy defaultChunkSize)
  .|  bracketP (IO.openBinaryFile f2 IO.WriteMode) IO.hClose sinkHandleFlush

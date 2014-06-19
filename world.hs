module World where

import Data

import Control.Concurrent.MVar
import Control.Exception
import System.IO
import qualified System.IO.Strict as Strict

loadWorld fileName defaultWorld = 
	(try (read `fmap` Strict.readFile fileName) :: IO (Either SomeException World)) >>= \l ->
  	case l of
    	Left _ 	-> putStrLn "Using default world." >> return defaultWorld
    	Right w -> putStrLn ("Loaded " ++ fileName) >> return w

saveWorld fileName world = 
	writeFile fileName (show world) 
	>> putStrLn ("Saved to " ++ fileName)

fWorld f box = 
	takeMVar box >>= \world ->
	f world >>= \_ ->
	putMVar box world
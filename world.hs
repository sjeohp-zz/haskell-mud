module World where

import Data

import Control.Concurrent.MVar
import Control.Exception
import System.IO
import qualified System.IO.Strict as Strict

defaultRoom = Room { rnum = 0, roomName = "Void", roomExits = [], roomUsers = [] }
defaultWorld = World { roomsAll = [defaultRoom], usersAll = [], usersConnected = [] }

loadWorld fileName defaultWorld = 
	(try (read `fmap` Strict.readFile fileName) :: IO (Either SomeException World)) >>= \l ->
  	case l of
    	Left _ 	-> putStrLn "Using default instanceOfWorld." >> return defaultWorld
    	Right w -> putStrLn ("Loaded " ++ fileName) >> return w

saveWorld fileName instanceOfWorld = 
	writeFile fileName (show instanceOfWorld) 
	>> putStrLn ("Saved to " ++ fileName)

fWorld f boxOfWorld = 
	takeMVar boxOfWorld >>= \instanceOfWorld ->
	f instanceOfWorld >>= \_ ->
	putMVar boxOfWorld instanceOfWorld
module Main (main) where

import World
import User
import Data

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix (fix)

type Msg = (Int, String)
type Cmd = (User, String)

saveFile = "save_instanceOfWorld.txt"

data Trie a = Leaf a | Branch [Trie a]

main = 
	loadWorld saveFile defaultWorld >>= \instanceOfWorld ->
	newMVar instanceOfWorld >>= \boxOfWorld ->
   	newChan >>= \chan -> 
	socket AF_INET Stream 0 >>= \sock -> 
	setSocketOption sock ReuseAddr 1
    >> bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    >> listen sock 2
    >> (forkIO $ fix $ \loop -> readChan chan >>= \(_, msg) -> loop)
    >> mainLoop boxOfWorld sock chan 0

mainLoop boxOfWorld sock chan nr =
	accept sock >>= \conn ->
	forkIO (runConn boxOfWorld conn chan nr)
	>> (mainLoop boxOfWorld sock chan $! nr + 1)

runConn boxOfWorld (sock, _) chan nr =
	let broadcast msg = writeChan chan (nr, msg) in
	
	socketToHandle sock ReadWriteMode >>= \hdl ->
	hSetBuffering hdl NoBuffering

	>> hPutStrLn hdl "Who are you?"
	>> liftM init (hGetLine hdl) >>= \name ->

	getUser name boxOfWorld >>= \user ->
	broadcast ("-->" ++ (userName user) ++ " entered.")
	>> hPutStrLn hdl ("Hi, " ++ (userName user) ++ ".")
	>> dupChan chan >>= \chan' ->

	readLoop chan' hdl nr >>= \reader ->
	userLoop chan' hdl nr user boxOfWorld

	>> takeMVar boxOfWorld >>= \instanceOfWorld ->
	saveWorld saveFile instanceOfWorld
	>> putMVar boxOfWorld instanceOfWorld

	>> killThread reader
	>> broadcast ("<--" ++ (userName user) ++ " left.")
	>> hClose hdl
	
readLoop chan hdl nr =
	forkIO $ fix $ \loop -> 
	(readChan chan >>= \(nr', line) -> when (nr /= nr') $ hPutStrLn hdl line)
	>> loop

userLoop chan hdl nr user boxOfWorld =
	handle (\(SomeException _) -> return ()) $ fix $ \loop ->
	liftM init (hGetLine hdl) >>= \line ->
	case line of
		"look"	-> (hPutStrLn hdl $ roomName $ userRoom user)
					>> loop
		"quit"	-> hPutStrLn hdl "Farewell"
					>> takeMVar boxOfWorld >>= \instanceOfWorld ->
					let instanceOfWorld' = disconnectUser user instanceOfWorld in 
					putMVar boxOfWorld instanceOfWorld'
		_		-> (writeChan chan (nr, ((userName user) ++ ": " ++ line)))
					>> loop










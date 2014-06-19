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

saveFile = "save_world.txt"
defaultRoom = Room { roomIndex = 0, roomName = "Lobby", roomExits = [] }
defaultWorld = World { rooms = [defaultRoom], users = [], connected = [] }

usersAll = []
usersConnected = []

main = 
	loadWorld saveFile defaultWorld >>= \world ->
	newMVar world >>= \box ->
   	newChan >>= \chan -> 
	socket AF_INET Stream 0 >>= \sock -> 
	setSocketOption sock ReuseAddr 1
    >> bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    >> listen sock 2
    >> (forkIO $ fix $ \loop -> readChan chan >>= \(_, msg) -> loop)
    >> mainLoop box sock chan 0

mainLoop box sock chan nr =
	accept sock >>= \conn ->
	forkIO (runConn box conn chan nr)
	>> (mainLoop box sock chan $! nr + 1)

runConn box (sock, _) chan nr =
	let broadcast msg = writeChan chan (nr, msg) in
	socketToHandle sock ReadWriteMode >>= \hdl ->
	hSetBuffering hdl NoBuffering
	>> hPutStrLn hdl "Who are you?"
	>> liftM init (hGetLine hdl) >>= \name ->
	getUser name box >>= \user ->
	broadcast ("-->" ++ (userName user) ++ " entered.")
	>> hPutStrLn hdl ("Hi, " ++ (userName user) ++ ".")
	>> dupChan chan >>= \chan' ->
	readLoop chan' hdl nr >>= \reader ->
	userLoop chan' hdl nr user box
	>> takeMVar box >>= \world ->
	saveWorld saveFile world
	>> putMVar box world
	>> killThread reader
	>> broadcast ("<--" ++ (userName user) ++ " left.")
	>> hClose hdl
	
readLoop chan hdl nr =
	forkIO $ fix $ \loop -> 
	(readChan chan >>= \(nr', line) -> when (nr /= nr') $ hPutStrLn hdl line)
	>> loop

userLoop chan hdl nr user box =
	handle (\(SomeException _) -> return ()) $ fix $ \loop ->
	liftM init (hGetLine hdl) >>= \line ->
	case line of
		"l"		-> (hPutStrLn hdl $ roomName $ userRoom user)
					>> loop
		"quit"	-> hPutStrLn hdl "Farewell"
					>> takeMVar box >>= \world ->
					let world' = disconnectUser user world in putMVar box world'
		_		-> (writeChan chan (nr, ((userName user) ++ ": " ++ line)))
					>> loop










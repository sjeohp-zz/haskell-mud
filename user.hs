module User where

import World
import Data

import Control.Concurrent.MVar

getUser name box =
	takeMVar box >>= \world ->
	getUser' name (users world) world >>= \user ->
	(if userIsConnected name $ connected world
		then putMVar box world
		else putMVar box (connectUser (setConnected user world) world))
	>> return user

userIsConnected name [] = False
userIsConnected name (x:xs) = 
	if userName x == name
		then True
		else False

getUser' name [] world = return $ createUser name world
getUser' name (x:xs) world = 
	if (userName x) == name
		then return x
		else getUser' name xs world

createUser name world = 
	User { userName = name, userCnctIndex = length (connected world), userRoom = head (rooms world) }

setConnected user world = 
	User { userName = userName user, userCnctIndex = length (connected world), userRoom = userRoom user }
connectUser user world = 
	World { rooms = rooms world, users = user:(users world), connected = user:(connected world) }

disconnectUser user world = 
	World { rooms = rooms world, users = users world, connected = disconnectUser' user (connected world) }
disconnectUser' user connected = 
	let (ys,zs) = splitAt (userCnctIndex user) connected in ys ++ (tail zs)
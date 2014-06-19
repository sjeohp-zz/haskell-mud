module User where

import World
import Data

import Control.Concurrent.MVar

getUser name boxOfWorld =
	takeMVar boxOfWorld >>= \instanceOfWorld ->
	getUser' name (usersAll instanceOfWorld) instanceOfWorld >>= \user ->
	(if userIsConnected name $ usersConnected instanceOfWorld
		then putMVar boxOfWorld instanceOfWorld
		else putMVar boxOfWorld (connectUser (setConnected user instanceOfWorld) instanceOfWorld))
	>> return user

getUser' name [] instanceOfWorld = return $ createUser name instanceOfWorld
getUser' name (x:xs) instanceOfWorld = 
	if (userName x) == name
		then return x
		else getUser' name xs instanceOfWorld

userIsConnected name [] = False
userIsConnected name (x:xs) = 
	if userName x == name
		then True
		else userIsConnected name xs

createUser name instanceOfWorld = 
	User { userName = name, connectionIndex = length (usersConnected instanceOfWorld), userRoom = head (roomsAll instanceOfWorld) }

setConnected user instanceOfWorld = 
	User { userName = userName user, connectionIndex = length (usersConnected instanceOfWorld), userRoom = userRoom user }
connectUser user instanceOfWorld = 
	World { roomsAll = roomsAll instanceOfWorld, usersAll = user:(usersAll instanceOfWorld), usersConnected = user:(usersConnected instanceOfWorld) }

disconnectUser user instanceOfWorld = 
	World { roomsAll = roomsAll instanceOfWorld, usersAll = usersAll instanceOfWorld, usersConnected = disconnectUser' user (usersConnected instanceOfWorld) }
disconnectUser' user usersConnected = 
	let (ys,zs) = splitAt (connectionIndex user) usersConnected in ys ++ (tail zs)
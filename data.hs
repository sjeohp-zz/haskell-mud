module Data where

data World =
	World {
	rooms :: [Room],
	users :: [User],
	connected :: [User]
	} deriving (Eq, Show, Read)

data User = 
	User {
	userName :: String,
	userCnctIndex :: Int,
	userRoom :: Room
	} deriving (Eq, Show, Read)

data Room =
	Room {
	roomIndex :: Int,
	roomName :: String,
	roomExits :: [Room]
	--roomUsers :: [User]
	} deriving (Eq, Show, Read)
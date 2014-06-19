module Data where

data World =
	World {
	roomsAll :: [Room],
	usersAll :: [User],
	usersConnected :: [User]
	} deriving (Eq, Show, Read)

data User = 
	User {
	userName :: String,
	userRoom :: Room,
	connectionIndex :: Int
	} deriving (Eq, Show, Read)

data Room =
	Room {
	rnum :: Int,
	roomName :: String,
	roomExits :: [Room],
	roomUsers :: [User]
	} deriving (Eq, Show, Read)
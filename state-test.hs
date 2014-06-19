
import Network.Socket
import System.IO
import qualified System.IO.Strict as Strict
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Monad.Trans
import Control.Monad.Trans.State

data World =
	World {
	users :: [User],
	rooms :: [Room]
	} deriving (Eq, Show, Read)
	
data Room =
	Room {
	roomIndex :: Int,
	roomName :: String,
	roomExits :: [Room]
	} deriving (Eq, Show, Read)
	
data User = 
	User {
	userName :: String,
	userRoom :: Room
	} deriving (Eq, Show, Read)

getPlayers = do
	n <- lift getLine
	w <- get
	
	if n == "end"
		then do
			put w
			return w
		else do
			put (n:w)
			getPlayers
			
go = do
	p <- evalStateT getPlayers []
	print p
	return p



main = evalStateT go1 []

go1 =
	newChan >>= \chan -> 
	socket AF_INET Stream 0 >>= \sock -> 
	setSocketOption sock ReuseAddr 1
    >> bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    >> listen sock 2
    >> (forkIO $ fix $ \loop -> readChan chan >>= \(_, msg) -> loop)
    >> mainLoop sock chan 0

mainLoop sock chan nr = 
	accept sock >>= \conn ->
	forkIO (runConn conn chan nr)
	>> (mainLoop sock chan $! nr + 1)

runConn (sock, _) chan nr =
	let broadcast msg = writeChan chan (nr, msg) in
	socketToHandle sock ReadWriteMode >>= \hdl ->
	hSetBuffering hdl NoBuffering
	>> hPutStrLn hdl "Who are you?"
	>> liftM init (hGetLine hdl) >>= \name ->
	get >>= \instanceOfWorld ->
	put (name:instanceOfWorld)
	>> get >>= \instanceOfWorld' ->
	print instanceOfWorld'
	>> return instanceOfWorld'


module SimpleReader where

import Data.Either
import Control.Monad
import Data.IORef
import System.Directory
import Data.Time.Clock
import Data.Dynamic
import qualified Control.Concurrent as CONC

import IReader
import ICompiler
import Events

readerNotify :: String -> IO ()
readerNotify message = putStrLn message

data ReaderState = ReaderState
	{ readerStopped :: Bool
	, currentFile   :: String
	, modifyTime    :: UTCTime
	} deriving (Eq, Show, Read)

instance Reactor ReaderState ReaderEvent () where
	reactorStoppedQ = readerStopped
	reactorDelayMS = const 100
	reactorProcess ctx = readerProcess

readerProcess :: ReaderState -> [ReaderEvent] -> IO (ReaderState, [Dynamic])
readerProcess reader events = do
	(state, rbuf) <- readerInterpretEvents reader events
	(state, addbuf) <- rereadfile state
	return (state, rbuf ++ addbuf)

readerInterpretEvents :: ReaderState -> [ReaderEvent] -> IO (ReaderState, [Dynamic])
readerInterpretEvents reader events = do
	(state, rbuf) <- loop reader [] events
	return (state, reverse rbuf)
	where
	loop :: ReaderState -> [Dynamic] -> [ReaderEvent] -> IO (ReaderState, [Dynamic])
	loop state buf [] = return (state, buf)
	loop state buf (x : xs) = case x of
		ReaderStopEvent ->
			return (state { readerStopped = True }, buf)
		SourceFilepathChanged newpath -> do
			newmtime <- getModificationTime newpath
			text <- readFile newpath
			withNewFile newmtime text
			where
			withNewFile newmtime text =
				loop newstate ((toDyn $ SourceFileUpdated text) : buf) xs
				where newstate = state { currentFile = newpath, modifyTime = newmtime }

rereadfile :: ReaderState -> IO (ReaderState, [Dynamic])
rereadfile state = do
	curtime <- getModificationTime (currentFile state)
	if (curtime <= (modifyTime state))
	then return (state, [])
	else do
		newmtime <- getModificationTime (currentFile state)
		text <- readFile (currentFile state)
		return (state { modifyTime = newmtime }, [toDyn $ SourceFileUpdated text])

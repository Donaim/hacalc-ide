
module SimpleReader where

import Data.Either
import Control.Monad
import Data.IORef
import System.Directory
import Data.Time.Clock
import Data.Dynamic
import qualified Control.Concurrent as CONC
import IReader
import Events

readerNotify :: String -> IO ()
readerNotify message = putStrLn message

data ReaderState = ReaderState
	{ currentFile   :: String
	, modifyTime    :: UTCTime
	, readerStopped :: Bool
	} deriving (Eq, Show, Read)

instance Reactor ReaderState ReaderEvent where
	reactorStoppedQ = readerStopped
	reactorDelayMS = const 100

	reactorProcess = processReaderEvents
	reactorProduce = produceReaderEvents

processReaderEvents :: ReaderState -> [ReaderEvent] -> IO ReaderState
processReaderEvents reader0 events0 = return $ loop reader0 events0
	where
	loop :: ReaderState -> [ReaderEvent] -> ReaderState
	loop state [] = state
	loop state (x : xs) = case x of
		ReaderStopEvent -> (state { readerStopped = True })

produceReaderEvents :: ReaderState -> IO [Dynamic]
produceReaderEvents state = undefined
	where

	-- loop :: String -> UTCTime -> IO ()
	-- loop oldfile mtime = do
	-- 	CONC.threadDelay (100 * 1000)

	-- 	sess <- readIORef (session ctx)
	-- 	newmtime <- getModificationTime (currentFile sess)

	-- 	unless (stopped sess) $
	-- 		if (currentFile sess /= oldfile || newmtime > mtime)
	-- 		then do
	-- 			text <- readFile (currentFile sess)

	-- 			case readRules text of
	-- 				Left errors ->
	-- 					readerNotify $ "Errors during parsing rules: " ++ show errors
	-- 				Right {} -> do
	-- 					readerNotify $ "Read new rules"
	-- 					atomicModifyIORef' (session ctx) (setNewText text)
	-- 					atomicModifyIORef' (changedq ctx) (\ b -> (True, ()))

	-- 			loop (currentFile sess) newmtime
	-- 		else do
	-- 			loop oldfile mtime

	-- setNewText :: String -> ReplSession -> (ReplSession, ())
	-- setNewText newText sess = (sess { ruleText = newText }, ())




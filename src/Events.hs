
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Events where

import Data.Dynamic
import Data.IORef
import Data.Either
import Control.Concurrent as CONC
import Control.Monad
import Control.Exception

data EventsBin = EventsBin
	{ ref :: IORef [[Dynamic]]
	}

eventsBinNew :: IO EventsBin
eventsBinNew = do
	r <- newIORef []
	return $ EventsBin { ref = r }

sendEvent :: (Typeable a, Show a) => EventsBin -> a -> IO ()
sendEvent bin event = ioRefStdAdd (ref bin) [toDyn event]

sendEvents :: (Typeable a, Show a) => EventsBin -> [a] -> IO ()
sendEvents bin events = ioRefStdAdd (ref bin) (map toDyn events)

recieveEvents :: (Typeable a) => EventsBin -> IO [a]
recieveEvents bin = ioRefStdGet (ref bin) chooser

chooser :: (Typeable a) => Dynamic -> Either Dynamic a
chooser d = case fromDynamic d of
	Nothing -> Left d
	Just x -> Right x

ioRefStdGet :: IORef [[a]] -> (a -> Either a b) -> IO [b]
ioRefStdGet ref chooser = atomicModifyIORef' ref (modify . partitionEithers . map chooser . concat)
	where modify (left, right) = (if null left then [] else [left], right)

ioRefStdAdd :: IORef [[a]] -> [a] -> IO ()
ioRefStdAdd ref new =
	if null new
	then return ()
	else atomicModifyIORef' ref (\ all -> (all ++ [new], ())) -- TODO: append in O(1)


class (Show r, Read r, Typeable e) => Reactor r e ctx | r -> e, r -> ctx where
	reactorStartFlag :: r -> r
	reactorStoppedQ  :: r -> Bool
	reactorDelayMS   :: r -> Int
	reactorProcess   :: ctx -> r -> [e] -> IO (r, [Dynamic])

	reactorNewCtx :: EventsBin -> r -> IO ctx

	reactorInit :: ctx -> r -> IO r
	reactorInit ctx r = return r

	reactorLoadState :: FilePath -> IO r
	reactorLoadState path = do
		txt <- readFile path
		return $ read txt

	reactorSaveState :: r -> FilePath -> IO ()
	reactorSaveState r path = writeFile path (show r)

reactorLoop :: (Reactor r e ctx) => EventsBin -> r -> IO r
reactorLoop ebin state0 = do
	ctx <- reactorNewCtx ebin state0
	inited <- reactorInit ctx state0
	let started = reactorStartFlag inited

	unless (reactorStoppedQ state0) $ do
		putStrLn $ "WARNING: reactor was running during stop. Reactor: " ++ show state0
	when (reactorStoppedQ started) $ do
		putStrLn $ "WARNING: reactor did not start. Reactor: " ++ show started

	with_init ctx started
	where
	with_init ctx inited = do
		(r, me) <- loop inited
		case me of
			Nothing -> return ()
			Just e -> putStrLn $ "Reactor fail: " ++ show e ++ " ; Reactor = " ++ show r
		return r
		where
		loop state = do
			CONC.threadDelay (reactorDelayMS state * 1000)
			events <- recieveEvents ebin

			r <- try $ reactorProcess ctx state events
			case r :: Either SomeException _ of
				Left e -> return (state, Just e)
				Right (newstate, responses) -> do
					unless (null responses) (ioRefStdAdd (ref ebin) (responses))

					if reactorStoppedQ newstate
					then return (newstate, Nothing)
					else loop newstate


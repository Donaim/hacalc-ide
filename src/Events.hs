
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Events where

import Data.Dynamic
import Data.IORef
import Data.Either
import Control.Concurrent as CONC
import Control.Monad

data EventsBin = EventsBin
	{ ref :: IORef [[Dynamic]]
	}

sendEvent :: (Typeable a) => EventsBin -> a -> IO ()
sendEvent bin event = ioRefStdAdd (ref bin) [toDyn event]

sendEvents :: (Typeable a) => EventsBin -> [a] -> IO ()
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


class (Show r, Read r, Typeable e) => Reactor r e | r -> e where
	reactorStoppedQ  :: r -> Bool
	reactorDelayMS   :: r -> Int

	reactorProcess   :: r -> [e] -> IO (r, [Dynamic])

reactorLoop :: (Reactor r e) => EventsBin -> r -> IO r
reactorLoop ebin state0 = loop state0
	where
	loop state = do
		CONC.threadDelay (reactorDelayMS state * 1000)
		events <- recieveEvents ebin
		(newstate, responses) <- reactorProcess state events

		unless (null responses) (ioRefStdAdd (ref ebin) (responses))

		if (reactorStoppedQ state)
		then return newstate
		else loop newstate

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception (finally)
import           Control.Monad (void, when)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import           Network (Socket, PortID(..), accept, listenOn)
import           System.Environment (getArgs)
import           System.IO


-- | Semaphore key
type Key = B.ByteString


-- | Shared semaphore mapping
type Semaphores = Map.Map Key (MVar (), Int)


-- | Allow us to print the semaphore mapping
instance Show (MVar ()) where
    show _ = "MVar ()"


-- | Parse a semaphore request
--
-- >>> readKey "a 1"
-- Just ("a", 1)
-- >>> readKey "a"
-- Nothing
readKey :: Key -> Maybe (Key, Int)
readKey msg = let (_, nstr) = B.breakEnd (==' ') msg
              in ((,) msg . fst) <$> B.readInt nstr


-- | Handle a client socket
handleClient :: MVar Semaphores -> Handle -> IO ()
handleClient box client = do
    line <- B.hGetLine client
    case readKey line of
        Nothing ->
            debugClient box client line >> say "bye"
        Just (key, n) ->
            if n > 0 then handle key n else badDave
  where
    handle key n = do
        blockClient box wait key n
        handleClient box client
    say = B.hPutStrLn client
    wait = say "go" >> B.hGetLine client
    badDave = say "I'm afraid I can't let you do that, Dave."


-- | Potentially make a client wait for a slot to do work
blockClient :: MVar Semaphores -> IO a -> Key -> Int -> IO ()
blockClient box wait key n = do
    (sem, c) <- modifyMVar box $ getSemaphore key n
    takeMVar' c sem
    _ <- finally wait $ do
        _ <- tryPutMVar sem ()
        modifyMVar_ box cleanup
    return ()
  where
    takeMVar' c = if c < 0 then takeMVar else void . tryTakeMVar
    cleanup = return . cleanupSemaphore key n


-- | Get reference to a semaphore, decrement the slot count,
--   or create if it doesnt exist
getSemaphore :: Key -> Int -> Semaphores -> IO (Semaphores, (MVar (),Int))
getSemaphore key n sems = case Map.updateLookupWithKey dec key sems of
    (Just x, sems1) -> return (sems1, x)
    (Nothing, sems1) -> do
        sem <- newEmptyMVar
        return (Map.insert key (sem, n-1) sems1, (sem, n-1))
  where dec _ (s,i) = Just (s, i-1)


-- | Increment slot count, deleting the semaphore if it's unheld
cleanupSemaphore :: Key -> Int -> Semaphores -> Semaphores
cleanupSemaphore key n = Map.update clean key
  where clean (s,x) = if x == (n-1) then Nothing else Just (s,x+1)


-- | Dump semaphores
debugClient :: MVar Semaphores -> Handle -> Key -> IO ()
debugClient box client line = when isDebug dump
  where dump = readMVar box >>= hPrint client
        isDebug = B.isPrefixOf "debug" line


-- | Accept and fork client threads
serve :: MVar Semaphores -> Socket -> IO ()
serve box server = do
    (client, _, _) <- accept server
    hSetBuffering client LineBuffering
    _ <- forkIO $ finally (handleClient box client) (hClose client)
    serve box server


main :: IO ()
main = do
    args <- getArgs
    let port = fromIntegral (read (head args) :: Integer)
    server <- listenOn $ PortNumber port
    putStrLn $ "listening on: " ++ show port
    box <- newMVar Map.empty
    serve box server

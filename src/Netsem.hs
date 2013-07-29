{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Main where

import           Control.Concurrent
import           Control.Exception (finally)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import           Network (Socket, PortID(..), accept, listenOn)
import           System.Environment (getArgs)
import           System.IO


type Key = B.ByteString
type Semaphores = Map.Map Key (MVar (), Int)


instance Show (MVar ()) where
    show _ = "MVar ()"


readKey :: Key -> Maybe (Key, Int)
readKey msg = let (_, nstr) = B.breakEnd (==' ') msg
              in fmap ((,) msg . fst) $ B.readInt nstr


handleClient :: MVar Semaphores -> Handle -> IO ()
handleClient box client = do
    line <- B.hGetLine client
    case readKey line of
        Nothing -> do debugClient box client line
                      B.hPutStrLn client "bye"
        Just (key,n) -> if n > 0 then do blockClient box wait key n
                                         handleClient box client
                                 else badDave
  where
    wait = B.hPutStrLn client "go" >> B.hGetLine client
    badDave = B.hPutStrLn client "I'm afraid I can't let you do that, Dave."


blockClient :: MVar Semaphores -> IO a -> Key -> Int -> IO ()
blockClient box wait key n = do
    (sem,c) <- modifyMVar box $ getSemaphore key n
    if c < 0 then takeMVar sem else tryTakeMVar sem >> return ()
    finally (wait >> return ()) $ do tryPutMVar sem ()
                                     modifyMVar_ box cleanup
  where cleanup = return . cleanupSemaphore key n


getSemaphore :: Key -> Int -> Semaphores -> IO (Semaphores, (MVar (),Int))
getSemaphore key n sems = case Map.updateLookupWithKey dec key sems of
    (Just x, sems1) -> return (sems1, x)
    (Nothing, sems1) -> do sem <- newEmptyMVar
                           return (Map.insert key (sem,n-1) sems1, (sem,n-1))
  where dec _ (s,i) = Just (s,i-1)


cleanupSemaphore :: Key -> Int -> Semaphores -> Semaphores
cleanupSemaphore key n sems = Map.update clean key sems
  where clean (s,x) = if x == (n-1) then Nothing else Just (s,x+1)


debugClient :: MVar Semaphores -> Handle -> Key -> IO ()
debugClient box client line = if isDebug then dump else return ()
  where dump = readMVar box >>= hPutStrLn client . show
        isDebug = B.isPrefixOf "debug" line


serve :: MVar Semaphores -> Socket -> IO ()
serve box server = do
    (client, _, _) <- accept server
    hSetBuffering client LineBuffering
    _ <- forkIO $ finally (handleClient box client) (hClose client)
    serve box server


main :: IO ()
main = do
    args <- getArgs
    let port = fromIntegral $ read $ head args
    server <- listenOn $ PortNumber port
    putStrLn $ "listening on: " ++ show port
    box <- newMVar Map.empty
    serve box server

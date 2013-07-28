{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.MSem (MSem, new, with)
import           Control.Exception (finally)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import           Network (Socket, PortID(..), accept, listenOn)
import           System.Environment (getArgs)
import           System.IO


type Key = B.ByteString
type Semaphores = Map.Map Key (MSem Int, Int)


instance Show (MSem Int) where
    show _ = "MSem"


readKey :: Key -> Maybe (Key, Int)
readKey msg = let (_, nstr) = B.breakEnd (==' ') msg
              in fmap ((,) msg . fst) $ B.readInt nstr


handleClient :: MVar Semaphores -> Handle -> IO ()
handleClient box client = do
    line <- B.hGetLine client
    case readKey line of
        Nothing -> B.hPutStrLn client "bye"
        Just (key,n) -> if n > 0 then do blockClient box wait key n
                                         handleClient box client
                                 else badDave
  where
    wait = B.hPutStrLn client "go" >> B.hGetLine client
    badDave = B.hPutStrLn client "I'm afraid I can't let you do that, Dave."


blockClient :: MVar Semaphores -> IO a -> Key -> Int -> IO ()
blockClient box wait key n = do
    sem <- modifyMVar box $ getSemaphore key n
    finally (with sem wait >> return ())
            (modifyMVar_ box (return . cleanupSemaphore key))


getSemaphore :: Key -> Int -> Semaphores -> IO (Semaphores, MSem Int)
getSemaphore key n sems = case Map.updateLookupWithKey inc key sems of
    (Just (sem,_), sems1) -> return (sems1, sem)
    (Nothing, sems1) -> do sem <- new n
                           return (Map.insert key (sem,1) sems1, sem)
    where inc _ (s,i) = Just (s,i+1)


cleanupSemaphore :: Key -> Semaphores -> Semaphores
cleanupSemaphore key sems = Map.update clean key sems
  where clean (s,x) = if x == 1 then Nothing else Just (s,x-1)


serve :: MVar Semaphores -> Socket -> IO ()
serve box server = do
    (client, _, _) <- accept server
    hSetBuffering client LineBuffering
    _ <- forkIO $ finally (handleClient box client) (hClose client)
    serve box server


debug :: MVar Semaphores -> IO ()
debug box = do
    threadDelay 1000000
    readMVar box >>= print
    debug box


main :: IO ()
main = do
    args <- getArgs
    let port = fromIntegral $ read $ head args
    server <- listenOn $ PortNumber port
    putStrLn $ "listening on: " ++ show port
    box <- newMVar Map.empty
    _ <- forkIO $ debug box
    serve box server

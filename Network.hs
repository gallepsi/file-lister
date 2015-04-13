module Network (connectToServer
               )
where

import System.ZMQ4.Monadic
import Control.Monad (forM_)
import Data.ByteString.Char8 (pack, unpack)

connectToServer :: ZMQ z ()
connectToServer =
    runZMQ $ do
        liftIO $ putStrLn "Connecting to Hello World server…"  
        reqSocket <- socket Req
        connect reqSocket "tcp://localhost:5555"
        return reqSocket

{-
main :: IO ()
main = 
    runZMQ $ do
        liftIO $ putStrLn "Connecting to Hello World server..."  
        reqSocket <- socket Req
        connect reqSocket "tcp://localhost:5555"
        forM_ [1..10] $ \i -> do
            liftIO $ putStrLn $ unwords ["Sending request", show i]
            send reqSocket [] (pack "Hello")
            reply <- receive reqSocket
            liftIO $ putStrLn $ unwords ["Received reply:", unpack reply]    
-}

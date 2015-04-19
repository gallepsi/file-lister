module ScanAllFiles(getStruct,
                    unwantedPath,
                    accessAllowed,
                    manageDirectory,
                    listAllFiles
                  )
where

import System.Directory 
import System.Posix
import System.IO
import System.Time
import System.Environment
import Control.Monad
import Data.List
import qualified Data.Set as S 
import Control.Exception
import Control.Concurrent


type FileStr =  (FilePath, String, Integer)
type Stream a = MVar (Item a)
data Item a   = Item a (Stream a)
  
data Channel a = Channel (MVar (Stream a)) (MVar (Stream a))

newChannel :: IO (Channel FileStr)
newChannel = do
  hole  <- newEmptyMVar
  readVar  <- newMVar hole
  writeVar <- newMVar hole
  return (Channel readVar writeVar)

writeChannel :: Channel FileStr -> FileStr -> IO ()
writeChannel (Channel _ writeVar) val = do
  newHole <- newEmptyMVar
  oldHole <- takeMVar writeVar
  putMVar oldHole (Item val newHole)
  putMVar writeVar newHole

readChannel :: Channel FileStr -> IO FileStr
readChannel (Channel readVar _) = do
  stream <- takeMVar readVar           
  Item val tail <- takeMVar stream      
  putMVar readVar tail                  
  return val

-- useless (more powerful nub)
eliminateDuplicates :: (Ord a) => [a] -> [a]
eliminateDuplicates = go S.empty
  where go _ [] = []
        go s (x:xs) | S.member x s = go s xs
                    | otherwise    = x : go (S.insert x s) xs


getTimes :: FilePath -> IO String
getTimes fp =
    do stat <- getFileStatus fp
       let a = (toct (accessTime stat)) 
       let r = calendarTimeToString(toUTCTime a)
       return r

toct :: EpochTime -> ClockTime
toct et = TOD (truncate (toRational et)) 0

getStruct :: FilePath -> Channel FileStr -> IO ()
getStruct [] chan =  return ()
getStruct f chan  = do
          -- Ultimate verification: trying to open the file & get his size
          sizeFile <-  try (withFile f ReadMode $ \h -> do hFileSize h >>= return) :: IO (Either SomeException Integer)
          case sizeFile of
              Left ex  ->  return ()
              Right sizeFile -> go sizeFile
                where 
                  go sizeFile = do
                      mt <- getTimes f
                      --let a = FileStr (f,mt,sizeFile)
                      writeChannel chan (f,mt,sizeFile)
                      return ()

unwantedPath :: [FilePath]
--unwantedPath = ["//Network","//dev","//Volumes","//root","//etc","//home","//Applications","//Users"]
unwantedPath = ["//Network",
                "//dev",
                "//Volumes"]

accessAllowed :: FilePath ->IO Bool
accessAllowed path = 
      if (path `elem` unwantedPath) then 
          return False
      else do
          status <- getFileStatus path
          doesFileExists <- fileExist path
          isAccessPermitted <- getPermissions path 
          return  (doesFileExists && (readable isAccessPermitted))

manageDirectory :: FilePath -> Channel FileStr -> DirStream -> String -> IO ()
manageDirectory _ _ dir []  = closeDirStream dir >>= \_ -> return ()
manageDirectory path chan dir  "." = readDirStream dir >>= manageDirectory path chan dir 
manageDirectory path chan dir  ".."= readDirStream dir >>= manageDirectory path chan dir 
manageDirectory path chan dir  file= 
                    let newpath = path++"/"++file in do
                      listAllFiles newpath chan
                      readDirStream dir >>= manageDirectory path chan dir
                      return ()

listAllFiles :: FilePath -> Channel FileStr -> IO ()
listAllFiles path chan = do
      isAccessAllowed <- try (accessAllowed path) :: IO (Either SomeException Bool)
      case isAccessAllowed of
        Left ex  -> return ()
        Right allowed -> go path allowed
          where 
            go path True = do
              file <- getSymbolicLinkStatus path
              if isDirectory file then do
                 dir <- openDirStream path
                 readDirStream dir >>= manageDirectory path chan dir
                 return ()
              else do
                forkIO (getStruct path chan)
                return ()
            go _ _ = return ()


loop :: Channel FileStr -> IO [FileStr]
loop chan = do
      str <- try (readChannel chan) :: IO (Either SomeException FileStr)
      case str of
        Left ex  -> return []
        Right str -> go str
          where 
            go str = do
            --print str
            strs <- loop chan
            return $ str:strs

main = do
  print "Creating new channel"
  chan <- newChannel
  print "Listing all files..."
  listAllFiles "/Users" chan
  print "Getting structure from channel..."
  lst <- loop chan
  print (head lst)
  print $ length lst
  print "end"


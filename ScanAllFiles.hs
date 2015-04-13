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

getStruct :: [FilePath] -> IO [(FilePath,String,Integer)]
getStruct [] =  return []
getStruct (f:fs) = do
          -- Ultimate verification: trying to open the file & get his size
          sizeFile <-  try (withFile f ReadMode $ \h -> do hFileSize h >>= return) :: IO (Either SomeException Integer)
          case sizeFile of
              Left ex  -> getStruct fs >>= return
              Right sizeFile -> go sizeFile
                where 
                  go sizeFile = do
                      mt <- getTimes f
                      recursiveStruct <- (getStruct fs)
                      return ((f,mt,sizeFile) : recursiveStruct)

unwantedPath :: [FilePath]
unwantedPath = ["//Network","//dev","//Volumes","//root","//etc","//home","//Applications","//Users"]

--unwantedPath = ["//Network",
--                "//dev",
--                "//Volumes"]

accessAllowed :: FilePath ->IO Bool
accessAllowed path = 
      if (path `elem` unwantedPath) then 
          return False
      else do
          status <- getFileStatus path
          doesFileExists <- fileExist path
          isAccessPermitted <- getPermissions path 
          return  (doesFileExists && (readable isAccessPermitted))

manageDirectory :: FilePath -> DirStream -> String -> IO [FilePath]
manageDirectory _ dir []  = closeDirStream dir >>= \_ -> return []
manageDirectory path dir "." = readDirStream dir >>= manageDirectory path dir
manageDirectory path dir ".."= readDirStream dir >>= manageDirectory path dir
manageDirectory path dir file= 
                    let newpath = path++"/"++file in do
                      allFiles <- listAllFiles newpath
                      allFiles' <- readDirStream dir >>= manageDirectory path dir
                      return (allFiles++allFiles')

listAllFiles :: FilePath -> IO [FilePath]
listAllFiles path = do
      isAccessAllowed <- try (accessAllowed path) :: IO (Either SomeException Bool)
      case isAccessAllowed of
        Left ex  -> return []
        Right allowed -> go path allowed
          where 
            go path True = do
              file <- getSymbolicLinkStatus path
              if isDirectory file then do
                 dir <- openDirStream path
                 allFiles <- readDirStream dir >>= manageDirectory path dir
                 return allFiles
              else do
                return [path]
            go _ _ = return []


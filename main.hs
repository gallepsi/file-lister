{-# LANGUAGE OverloadedStrings #-}

import Json
import ScanAllFiles
--import Network
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson

ext :: Maybe Datas -> Datas
ext (Just(d)) = d
ext Nothing = error "fail"

main :: IO ()
main = do
  --let req = decodeFromJSON "{\"allFiles\":[{\"path\":\"/root\",\"sizeFile\":134,\"nameFile\":\"file1.exe\"}],\"idClient\":1}" :: Maybe Datas
  --print (idClient (ext(req)))
  --let reply = Datas{ idClient=1, allFiles=[ File{path="/root",nameFile="file1.exe",sizeFile=134},File{path="/root",nameFile="file2.exe",sizeFile=134}] }
  --BL.putStrLn (encodeToJSON reply)
  print "-- Scanning system --"
  lst <- listAllFiles "/"
  print "Scan done, preview Struct:"
  a <- getStruct lst
  --let b = listFiles_to_data a in do
  --let reply = Datas{ idClient=1, allFiles=b }
  return ()
  --BL.putStrLn (encode reply)
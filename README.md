# file-lister

Program fully written in Haskell which will be able to :
  - Scan all files from your computer (unix only for the moment)
  - Create a Json structure: { [(pathFile, sizeFile, lastModificationFile)] , idUser}
  - Send this Json Structure to a remote server via ZMQ
  - Server Side program analyses the files

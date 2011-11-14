{-
Quentin Crisp: Unix has several weak points but its filesystem is not one of them
-}
module Fs (
  showDirectoryList,
  filteredDirList,
  cat,
  number_lines,
  wc,
  withContent,
  withLine,
  withLines
  ) where

import Common
import Strings as S
import Data.List
import System.IO
import System.Cmd
import System.Directory
import System.IO.Error

showDirectoryList list = putStrLn $ concatMap (++"\n") $ list

filteredDirList dir  = do filepaths <- getDirectoryContents dir
                          return $ filter filterBy filepaths
  where
    tempFileP = (isInfixOf "~" )
    hiddenFileP = (isPrefixOf ".")
    directoryP =  (== ".")
    parentP =  (== ".")
    negfilterList = [tempFileP, hiddenFileP ,directoryP , parentP]
    filterBy filePath = andP (notP negfilterList)  $  filePath


withContent fileName action =do h <- catch (openFile fileName ReadMode) errorHandler
                                content <- hGetContents h
                                action content 
  where 
    errorHandler = (\e -> do ioError (userError ("Cannot Open File "++ fileName ++ ", you fool !\n" ++ show e)))


withLine filename lineFunction = withContent filename linesFunction
  where linesFunction lines =  return $ map lineFunction lines

withLines filename linesFunction = withContent filename contentAction
  where contentAction content = return $ linesFunction (lines content)         

-- filteLines is a poor man's grep
filterLines filename filterFunction  = withLines filename $ filter (filterFunction)

--Gah!  you cant beat text streams | command interpretor combination<
--Some suble bug here always gives me size 0 instead of telling me the true type.

cat filename = withContent filename action
  where action content = putStr $ content ++"\n"  

wc filename = withContent filename action 
  where action content = putStrLn (foldl (\line acc -> line++"\n"++acc) "\n" (number_lines content))
        
        
cat1 filename = withContent filename action
  where 
    action content = putStr $ content ++ "\n"

number_lines content = number_lines' (lines content) 1
  where 
    number_lines' [] a = []
    number_lines' (l:ls) a = ((show a ) ++ " : "++ l ): number_lines' ls (a+1)


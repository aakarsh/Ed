{- H.E.D -Haskell's e.d -}
module Ed
       where       

import System.Cmd
import qualified Data.Map as M
import Control.Monad
import Data.Maybe
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import Common
import MarkedList
import Fs
import Geometry
import Statistics
import Data.List
import System.IO
import System.Cmd
import System.Directory
import Control.Exception
import Strings as S

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
       
data EdCmd = EdSubstitue {old,new::String,global::Maybe Bool}
           | EdOpenFile {filename::String}
           | EdPrintFile {filename::String}
           | EdSystemCmd {syscmd::String}
           | EdCountLines {filename::String}
           | EdWriteFile
           | EdUnrecognized {cmd ::Char,line::String }
           | EdQuit


parseLine [] = EdUnrecognized ' ' ""
parseLine line@(l:ls) 
  | l == 'q' = EdQuit
  | l == '!' = EdSystemCmd $ param ls
  | l == 'o' = EdOpenFile $ param ls  
  | l == 'w' = EdWriteFile
  | l == 'c' = EdCountLines $ param ls
  | l == 'p' = EdPrintFile $ param ls
  | l == 's' = let cmdArgs = S.splitOn ls '/'
               in EdSubstitue {old = cmdArgs!!0,new=cmdArgs!!1 ,global=Just True}
  | otherwise = EdUnrecognized l line -- nooop for now 
  where
    param = delNewLine . delStartSpace 
    delStartSpace  = dropWhile  isSpace 
    delNewLine = takeWhile $ (not . ('\n' ==))
               
ed =  do putStrLn "Welcome to Ed ! "                  
         edLoop           

-- gah every time i need to add a cmd i need to edit this shit
-- Need Some form of map from type to haskell cmd
edLoop = do edCmd <- edPrompt 
            -- the problem here is the method of paring commands is very brittle?
            -- How do you go from a string to a intered symbol?
            -- How do you do autocompletions.
            let cmd = parseLine edCmd
              in case cmd of 
              (EdSubstitue old new global) -> 
                do putStrLn $ "Got Substition " ++old ++ " with "++new ; 
                   edLoop
              (EdOpenFile file) -> 
                do putStrLn $ "Got Open file " ++ show file;
                   cat file
                   edLoop;
              (EdPrintFile file) ->
                do cat file;
                   edLoop;
              (EdSystemCmd cmd) -> --r <-rawSystem cmd 
                do putStrLn $ "Exited With "++show 1;
                   edLoop
              (EdCountLines file) ->
                do wc file
                   edLoop
              EdQuit ->  -- no edLoop here
                do putStrLn "Thank you,come again!"                
              EdUnrecognized cmd line -> 
                do putStrLn $ "Unrecognized Command : "++show cmd  ++ "in line : "++line; 
                   putStrLn "Use q to quit."
                   edLoop;

edPrompt = do putStr ">"               
              flush
              line <- getLine
              return line
  where
    flush = hFlush stdout

main :: IO()    
main = ed  

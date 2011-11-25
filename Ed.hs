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

data Tree a = Branch a (Tree a) (Tree a)| Leaf a
--data Cons a = Cons ( |  StackEnd
--[]

basic_tree 1 = (Branch 1 (Branch 2 (Leaf 1) (Leaf 3)) (Leaf 1))
basic_tree 2 = (Branch 1 (Leaf 2) (Branch 2  (Branch 2 (Leaf 1) (Leaf 3)) (Leaf 1)))

tree_sum (Branch v left right ) =  v+(tree_sum left) + (tree_sum right)
tree_sum (Leaf v ) = v

--tree_fold f i (Branch v left right ) =  f v (tree_fold f i left) (tree_fold f i right)
--tree_fold f  i (Leaf  v ) =  f v i
count_input_len = do line <- fmap (length) getLine
                     putStrLn $ "You entered "++(show line )++ " characters"

instance (Show a)=> Show (Tree a) where
     show (Branch a left right ) =show a++":(" ++ show left ++ ","++show right ++ ")"
     show (Leaf a) = show a

instance Functor  (Tree)  where
         fmap f (Leaf a) = Leaf $ f a
         fmap f (Branch a left right) = Branch (f a) (fmap f left) (fmap f right)


forever' pre x post = do pre; 
                         x; 
                         post;
                         forever' pre x post
                
confirm what = do putStr $ "Are you sure you want to "++what ++" [y/n] "; 
                  (y:ys) <- getLine;
                  if y == 'y' || y == 'Y'
                    then
                    return True
                    else
                    return False
               
genie_ask = putStrLn "What do you want master ?" 
genie_inform = putStrLn "Done!! Wish your wish has be fulfilled!";
genie x = forever' genie_ask (putStrLn "") genie_inform

repeat' 0 action = return ()
repeat' n action = action >> repeat' (n-1) action

for [] fa = return ()
for (n:ns) fa = fa n >> for ns fa

print_alphabet = for (['a'..'z']++"\n") (putChar)
{-
take_chars (c:[]) = do return  c:[]
take_chars (c:cs)= do c <-getChar
                      r<-fmap (c:) (take_chars cs)
                      return fmap (c:) r
-}
{--
collect' 0 action = return []
collect' n action = action>>== result : (collect' (n-1) action)
-}

{-
doubting_genie = forever' ask (putStrLn) inform
  where ask = do { genie_ask ;
                   answer <- getLine;
                   return $confirm answer;
                   }
        inform =  genie_inform
-}                   
                            
        
main :: IO()    
main = ed  

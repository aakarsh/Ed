{-
   Yoda: Do or do not there is no try.
-}
module Do where 

import qualified Data.Map as M
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
{-
import Text.ParserCombinators.Parsec

--parseCSV :: String -> Either ParseError [[[String]]]
parseCSV input = parse csvFile "(unknown)" input
csvFile = do result <- many line
             eof
             return result

eol = do char '\n'
         char '\r' <|> return '\n'
  

line::GenParser Char st [String]
line = do result <- cells             
          eol
          return result
          
cellContent::GenParser Char st String
cellContent = many (noneOf ",\n")

cells:: GenParser Char st [String] 
cells = do first <- cellContent 
           next <- remainingCells
           return (first:next)

remainingCells:: GenParser Char st [String] 
remainingCells = (char ',' >> cells) 
                 <|> (return [])

-}
--eol::GenParser Char st Char
--eol = string "\n" 



type CardHolder = String
type CardNumber = String
type Address = [String]

data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String] 
                    deriving (Show)
                             
myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]
type CustomerID = Int
type ReviewBody = String         
data BookReview = BookReview BookInfo CustomerID String         
type BookRecord = (BookInfo, BookReview)

data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

mylen [] = 0
mylen (x:xs) = 1+ mylen xs

mysum [] = 0
mysum (x:xs) = x + (mysum xs)

mean [] = 0
mean x = (mysum x) / fromIntegral (length x)

surround e [] = [e,e]
surround e l = [e]++l++[e]  

palinify [] = []
palinify (x:xs)  = surround x (palinify xs)


-- how does this work.
--ispalin [] = True
--ispalin (x:xs:y:[]) = (x==y) and (ispalin xs)
                   
myfact n    
  | n == 0    = 1
  | otherwise = n * myfact(n-1)
-- ghci> let cities = Book 173 "Use of Weapons" ["Iain M. Banks"]         


myDrop n xs = if n <= 0  || null xs                 
              then xs
              else  myDrop (n-1) (tail xs)
                    
myHead n [] = [] 
myHead n (x:xs) = if n <= 0   
                  then [] 
                  else x : (myHead (n-1) xs)


squares' n i
  | i >= n = (n*n):[]
  | otherwise = (i*i) : (squares' n (i+1))


square n = n^2
p_map p = map (\x -> x^p)            
sum' = foldl (\x y -> x + y) 0 


abs' x = if x < 0 
           then -x
           else x

fib' 0 = 1
fib' 1 = 1 
fib' n = fib' (n-1) + fib' (n-2)

fib = 1:1 : [a+b | (a,b) <- zip fib (tail fib) ]
ones = 1 : ones

number_sequence_from   n = n : number_sequence_from (n+1)
number_sequence = number_sequence_from 1
sign x |  x < 0   = -1
       |  x == 0  = 0           
       |  x > 0   = 1
hang = hang

data Tree a = Leaf a | Branch (Tree a) (Tree a)
--  fmap f (Branch r l) = (fmap f r ) (fmap f l)                
instance Functor Tree where
  fmap f (Leaf v) = Leaf (f v )                    
  fmap f (Branch l r)= Branch (fmap f l) (fmap f r)
  

readName = do putStr "Enter name : "
              name <- getLine
              return name
               
yesNo prompt = do putStr $ prompt ++ " [y/n] : "
                  c <-getLine
                  return (c == "y")                  
                  
greet = do yes <- (yesNo "Have a name")
           if yes 
             then do name <- (readName)
                     putStr $ "Hey! "++ name ++ "\n"                                     
             else return ()                  
-- do { contents <- ((openFile "/etc/passwd" ReadMode) >>= hGetContents ); putStr contents}
-- do { contents <- ((openFile "/etc/passwd" ReadMode) >>= hGetContents ); putStr $ "Length : "++ (show $ (length contents)) ++  "\n"}
-- cat filename = 
--   do h <- (openFile filename ReadMode) 
--      content <- hGetContents h
--      putStr $ content ++ "\n"                    

os_messages = cat "/var/log/messages"
os_proc_vmstat = cat "/proc/vmstat"



{- 
Prime numbers are recursive in the sense
that their previous input is everything 
that got missed by their current.
Seive Works by dropping ever nth where nths 
is the previous run of sieve on the input
the seive is done when 
-}

{-
   Believe it or not this implements prime seive we mark all composites up the primes.
-}
mark_composites [] = []
mark_composites list@(l:ls) = l : mark_composites (mark_every (fst l) ls)
seive_primes l  = fromMarked(filterUnMarked(mark_composites (unMarked l)))
primes = seive_primes [2..]

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n = n: (collatz (next n))
  where next n 
          | odd n = n*3+1
          | otherwise = n `div` 2

-- back tracking and propagation networks.
-- the issue is drop every is issuing drops on new size list
-- while we want to drop on the index position of the old list
  
--fibs_from f1 f2  = fibs_from
-- mode l = ?
--median l =  ((fromIntegral (length l)) / 2)
{-- Some gap in knowledge here instance Show Tree a where show t =
rcase t of (Leaf v) -> "("++ show v ++")" (Branch l r) -> "("++show l++
"," ++ show r ")"
--}

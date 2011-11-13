module Do (
  primes
  ) where 

import qualified Data.Map as M
import Common
import Data.List
import System.IO
import System.IO.Error
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

--eol::GenParser Char st Char
--eol = string "\n" 


{-
   Euclid : There is no royal road to geometry
-}
data Point = Pt {pointx ,pointy ::Double}

instance Eq Point where 
  (==) (Pt x1 y1) (Pt x2 y2) = (x1 == x2) && (y1 == y2 )

pointDistance (Pt x1 y1) (Pt x2 y2)  = sqrt $ (x1 - x2)^2 +  (y1 -y2)^2

data Circle = Circle {
  radius:: Double,
  origin ::Point
}

-- What about triangles where we can construct invalid types of triangles
-- Shouldnt I be able to specify some constraints onthe kinds of triangle i can create
{-
data Triangle = Triangle {
   point1,point2,point3 :: Point
}
-}

circleEq (Circle r (Pt {pointx = x, pointy = y})) = "(x-"++show x++") ^2 "++"+"++"(y-"++show y++" )^2 = "++(show  $ r^2)                                                   
cricleArea c = pi * (radius c)^2

cirlceCircumference c  =  2 * pi * (radius c)
circlesCoincide c1 c2 = (origin c1 == origin c2)

-- we consider coincidence not to be an intersection
circlesIntersect c1 c2 = (not ((radius c1 /= radius c2) && (circlesCoincide c1 c2)))  && (not ((pointDistance (origin c1) (origin c2)) > (radius c1 + radius c2)))
-- Determine if two circles intersect.
--intersection_points c1 c2 =

instance Show Circle where              
  show = circleEq 

instance Eq Circle where
  (==) (Circle r1 (Pt {pointx = x1 , pointy = y1}))  (Circle r2 (Pt {pointx = x2, pointy= y2})) =  r1 == r2 && x1 == x2 && y1 == y2
  
instance Ord Circle where  
  (>) (Circle r1 _ ) (Circle r2 _  )  = r1 > r2
  (<) (Circle r1 _ ) (Circle r2 _  )  = r1 < r2

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

--length is messed up returns Int                
length' l  = fromIntegral $ length l
mean' l =  (sum l) / (length' l)
mean_2 l1 l2 = mean' [l1,l2]

{-- Lots of stumbling blocks with length being int.--}
median [] = error "Empty list has no median"
median l  =  let sorted_list = sort l 
                 len =  (length sorted_list)                        
                 middle = floor $ fromIntegral len  / 2
             in
              if even len
              then 
                mean_2 (sorted_list !! (middle - 1) ) (sorted_list !! (middle))
              else
                sorted_list !! middle 
{-- 
Does not work very well for multimodal
--}
mode :: Ord a => [a] -> a
mode l = let frequency_map = M.toList (mode' l M.empty )
             frequency_compare y1 y2 = compare (snd y1) (snd y2)
         in 
          fst $ maximumBy frequency_compare frequency_map
  where
    mode' [] freq_map = freq_map
    mode' (l:ls) freq_map = mode' ls (M.insertWith' (+) l 1 freq_map)


standard_deviation [] =  0 
standard_deviation l = let mean = mean' l
                           len = length' l
                           mean_diff x = (x-mean)^2
                       in
                        sqrt $ (sum $ map mean_diff l )  /  len

range l = maximum l  - minimum l

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
                  
cat filename = withFile filename ReadMode hCat
  where  hCat h  = do content <- hGetContents h
                      putStr $ content ++"\n"

-- learn how to use shows clause
number_lines content = number_lines' (lines content) 1
  where 
    number_lines' [] a = []
    number_lines' (l:ls) a = ((show a ) ++ " : "++ l ): number_lines' ls (a+1)

-- some suble bug here always gives me size 0 instead of telling me the true type.
wc filename = withFile filename ReadMode hWc
  where hWc h = do content <- hGetContents h
                   putStrLn (foldl (\line acc -> line++"\n"++acc) "\n" (number_lines content))


cat1 filename = 
  do h <- catch (openFile filename ReadMode) errorHandler
     content <- hGetContents h
     putStr $ content ++ "\n"                    
  where 
    errorHandler = (\e -> do ioError (userError ("Cannot Open File "++ filename ++ ", you fool !\n" ++ show e)))

-- do { contents <- ((openFile "/etc/passwd" ReadMode) >>= hGetContents ); putStr contents}
-- do { contents <- ((openFile "/etc/passwd" ReadMode) >>= hGetContents ); putStr $ "Length : "++ (show $ (length contents)) ++  "\n"}
-- cat filename = 
--   do h <- (openFile filename ReadMode) 
--      content <- hGetContents h
--      putStr $ content ++ "\n"                    

os_messages = cat "/var/log/messages"
os_proc_vmstat = cat "/proc/vmstat"

main :: IO()  
main = greet

--returns an function which will apply f n times to its value
ntimes f n = fold_i_last (\previous _ -> f previous ) [1..n]
  where
    fold_i_last folder = flip $ foldl folder

applyTwice f = ntimes f 2

-- if we want to build prime number list we 
--need to  make sure number is not divisible till 
-- ceil (sqrt n)
-- so we want to build a filter function
-- which short circuits till sqrt of n
non_divisble a b =  (a `mod` b ) /=0
--seive i [] = []
--seive i l  = filter l 
ndrop n [] = []
ndrop 1 (l:ls) = ls
ndrop n (l:ls) = l : ndrop (n-1) ls

drop_every n l =  drop_every' n l 1
  where  drop_every' n [] i = []
         drop_every' n (l:ls) i = if i == n
                                  then drop_every' n ls 1
                                  else l:(drop_every' n ls (i+1))


{- Prime numbers are recursive in the sense
that their previous input is everything 
that got missed by their current.
Seive Works by dropping ever nth where nths 
is the previous run of sieve on the input
the seive is done when -}
mark (a,_)=(a,True)
mark_every n l = fevery mark n l

zipWithBool = zipWith (flip (,)) (repeat False)
filterBoolList l = filter (\(x,y)-> y) l
toBoolList = zipWithBool
fromBoolList = map (\(x,y) -> x ) 
notBoolList =  map (\(x,y) -> (x,(not y)))

mark_composites [] = []
mark_composites list@(l:ls) = l : mark_composites (mark_every (fst l) ls)
primes l  = fromBoolList (filterBoolList(notBoolList (mark_composites (toBoolList l))))


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
{-- 
Some gap in knowledge here 
instance Show Tree a where
  show t = case t of 
               (Leaf v) -> "("++ show v ++")"
               (Branch l r) -> "("++show l++ "," ++ show r ")"
--}

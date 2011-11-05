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
-- mylen :: [t] -> t1
mylen (x:xs) = 1+ mylen xs

mysum [] = 0
mysum (x:xs) = x + (mysum xs)

mean [] = 0
mean x = (mysum x) / fromIntegral (length x)
-- tries to covert a simple list such as [1,2,3] to a palindorme list such as [1,2,3,2,1]  
-- [1,2] [1,2,1]

surround e [] = [e]
surround e l = [e]++l++[e]
  
-- try palinify "malay" XD
palinify [] = []
palinify (x:xs)  = surround x (palinify xs)

myfact n    
  | n == 0    = 1
  | otherwise = n * myfact(n-1)
-- ghci> let cities = Book 173 "Use of Weapons" ["Iain M. Banks"]         

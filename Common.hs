module Common where

{-- 
Function call every nth entry
Takes a list and does something to ever nth term 
Returns back the old list
--}
fevery f n l  =  fevery' n l f 1
  where  fevery' n [] f i = []
         fevery' n (l:ls) f i = if i == n
                                  then (f l):(fevery' n ls f 1)
                                  else l:(fevery' n ls f (i+1))
                                       
selectEvery n l = selectEvery' n l 1
  where selectEvery' n [] i = [] 
        selectEvery' n (l:ls) i = if i == n 
                                  then l:(selectEvery' n ls 1)
                                  else (selectEvery' n ls  (i+1))
                                       
                                       
                                       
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

                                       


{- 
  We apply each function to the input then apply the joinBy function to coalesce the list. 
-}
applyFunctionsJoin joinBy functionList input =  joinBy $ zipWith ($) functionList  $ repeat input  
{- Ways to combine predicate function lists -}
andP  predicates input =  applyFunctionsJoin and  predicates input
orP  predicates input =  applyFunctionsJoin or predicates input
notP = map (not .) 
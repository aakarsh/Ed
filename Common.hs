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
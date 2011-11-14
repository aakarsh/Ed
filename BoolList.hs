module MarkedList(
  fromMarkedList,
  filterMarkedList,
  notMarkedList,
  mark_composites,
  toMarkedList,
  ) where
import Common

mark (a,_)=(a,True)
mark_every n l = fevery mark n l
zipWithBool = zipWith (flip (,)) (repeat False)
filterMarkedList l = filter (\(x,y)-> y) l
toMarkedList = zipWithBool
fromMarkedList = map (\(x,y) -> x ) 
notMarkedList =  map (\(x,y) -> (x,(not y)))
mark_composites [] = []
mark_composites list@(l:ls) = l : mark_composites (mark_every (fst l) ls)
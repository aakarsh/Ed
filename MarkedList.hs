module MarkedList(
  fromMarked,
  filterMarked,
  filterUnMarked,
  notMarked,
  unMarked,
  mark_every,
  mark,
  ) where
import Common

mark (a,_)=(a,True)
mark_every n l = fevery mark n l
zipWithBool = zipWith (flip (,)) (repeat False)
filterMarked l = filter (\(x,y)-> y) l
filterUnMarked  = filterMarked . notMarked 
unMarked = zipWithBool
fromMarked = map (\(x,y) -> x ) 
notMarked =  map (\(x,y) -> (x,(not y)))

{- 
Andrew Lang : An unsophisticated forecaster uses statistics as a drunken man uses
             lamp-posts - for support rather than for illumination.
-}
module Statistics (
     length' ,
     median ,
     mode ,
     standard_deviation ,
     range ,
     group_by_frequency,
  ) where 

import qualified Data.Map as M
import Common
import Strings as S
import Data.List
import System.IO
import System.IO.Error



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

frequency_map l = M.toList $ frequency_map' l M.empty
  where
    frequency_map' [] map = map
    frequency_map' list@(l:ls) map = frequency_map' ls (M.insertWith' (+) l 1 map)

mode :: Ord a => [a] -> a
mode l = let frequency_compare y1 y2 = compare (snd y1) (snd y2)
         in 
          fst $ maximumBy frequency_compare $ frequency_map l

group_by_frequency l = groupBy (\x y  -> (snd x) == (snd y))   $ frequency_map l

-- where is there support for stemming
-- this is all such bull shit reinventing the wheel badly
word_frequency content = group_by_frequency  $ (map S.toLower )$ words content

standard_deviation [] =  0 
standard_deviation l = let mean = mean' l
                           len = length' l
                           mean_diff x = (x-mean)^2
                       in
                        sqrt $ (sum $ map mean_diff l )  /  len

range l = maximum l  - minimum l

module Statistics (
     length' ,
     median ,
     mode ,
     standard_deviation ,
     range ,
  ) where 

import qualified Data.Map as M
import Common
import Data.List
import System.IO
import System.IO.Error
import Text.ParserCombinators.Parsec


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

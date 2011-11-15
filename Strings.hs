module Strings 
       where


import Data.Char as C
import Data.List as L
import Common 


toUpper = map C.toUpper 
toLower = map C.toLower
contains s = L.isInfixOf s
startsWith s = L.isPrefixOf s
endsWith s = L.isSuffixOf s


splitOn s b = splitWith s [b]
splitWith "" b = []
splitWith s b = let (l,s') = break (orP $ map (==) b) s
                in  
                 l : case s' of 
                   [] ->[]
                   (_:s'') -> splitWith s'' b
                  
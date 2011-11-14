module Strings 
       where

import Data.Char as C;
import Data.List as L;

toUpper = map C.toUpper 
toLower = map C.toLower
contains s = L.isInfixOf s
startsWith s = L.isPrefixOf s
endsWith s = L.isSuffixOf s
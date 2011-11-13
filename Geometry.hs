{-
   Euclid : There is no royal road to geometry
-}
module Geometry (
     Circle(..) ,
     Point(..) ,
     pointDistance ,
     circleEq ,
     cricleArea ,
     cirlceCircumference ,
     circlesCoincide ,
     circlesIntersect
    ) where


import qualified Data.Map as M
import Common
import Data.List
import System.IO
import System.IO.Error
import Text.ParserCombinators.Parsec


data Point = Pt {pointx ,pointy::Double}

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




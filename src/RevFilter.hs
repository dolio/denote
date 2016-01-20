
module RevFilter where

revFilter1 :: (a -> Bool) -> [a] -> [a]
revFilter1 p = loop []
 where
 loop acc [    ] = acc
 loop acc (x:xs) =
   if p x
   then loop (x:acc) xs
   else loop acc xs

revFilter2 :: (a -> Bool) -> [a] -> [a]
revFilter2 p = loop []
 where
 loop acc [    ] = acc
 loop acc (x:xs) = loop (if p x then x:acc else acc) xs

test'list :: [Int]
test'list = [1,2,3,4,8,undefined,9,10,11,12]

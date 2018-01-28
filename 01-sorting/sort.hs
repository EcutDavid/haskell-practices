-- insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort (firEle:listLeft) = insert firEle (insertSort listLeft)
    where insert a [] = [a]
          insert a (b:c)
            | a < b = a : b : c
            | otherwise = b : insert a c


quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:listLeft) = smallerOnes ++ [x] ++ largerOnes
    where smallerOnes = quickSort [ele | ele <- listLeft, ele <= x]
          largerOnes = quickSort [ele | ele <- listLeft, ele > x]

main :: IO()
main = do
  print (insertSort [1,23,123,12,312,31,23,12,1,12,312,312,3])
  print (quickSort [1,23,123,12,312,31,23,12,1,12,312,312,3])
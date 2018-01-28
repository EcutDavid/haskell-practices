insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort (firEle:arrLeft) = insert firEle (insertSort arrLeft)
    where insert a [] = [a]
          insert a (b:c)
            | a < b = a : b : c
            | otherwise = b : insert a c

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:y:arrLeft) = bubbleSort(init bubbled) ++ [last bubbled]
    where (smaller,bigger) = if(x <= y) then (x, y) else (y, x)
          bubbled = [smaller] ++ bubbleSort (bigger:arrLeft)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:arrLeft) = smallerOnes ++ [x] ++ largerOnes
    where smallerOnes = quickSort [ele | ele <- arrLeft, ele <= x]
          largerOnes = quickSort [ele | ele <- arrLeft, ele > x]

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort arr = merge(firHalfArr, secHalfArr)
    where arrHalfLength = div (length arr) 2
          firHalfArr = mergeSort (take arrHalfLength arr)
          secHalfArr = mergeSort (drop arrHalfLength arr)
          merge (a, []) = a
          merge ([], b) = b
          merge ((a1:aLeft), (b1:bLeft))
            | a1 <= b1 = a1:(merge (aLeft, (b1:bLeft)))
            | otherwise = b1:(merge (bLeft, (a1:aLeft)))

main :: IO()
main = do
  print (insertSort [1,23,123,12,312,31,23,12,1,12,312,312,3])
  print (quickSort [1,23,123,12,312,31,23,12,1,12,312,312,3])
  print (bubbleSort [1,23,123,12,312,31,23,12,1,12,312,312,3])
  print (mergeSort [1,23,123,12,312,31,23,12,1,12,312,312,3])

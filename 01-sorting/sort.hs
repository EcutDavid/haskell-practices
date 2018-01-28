insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort (firEle:listLeft) = insert firEle (insertSort listLeft)
    where insert a [] = [a]
          insert a (b:c)
            | a < b = a : b : c
            | otherwise = b : insert a c

main :: IO()
main = do
  print (insertSort [1,23,123,12,312,31,23,12,1])
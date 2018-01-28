main :: IO()

insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort [x] = [x]
insertSort (firEle:listLeft) = insert firEle (insertSort listLeft)
    where
        insert a (b:c)
            | [b] == [] = [a]
            | a < b = a : b : c
            | c == [] = b : [a]
            | otherwise = b : insert a c

main = do
  print (insertSort [1,23,123,12,312,31,23,12,1])
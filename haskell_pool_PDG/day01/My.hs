my42 :: Int
my42 = 42

mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x | x < 0 = True
          | otherwise = False

myAbs :: Int -> Int
myAbs x = case myIsNeg x of
  True -> -x
  False -> x

myMin :: Int -> Int -> Int
myMin x x'  | x < x' = x
            | otherwise = x'

myMax :: Int -> Int -> Int
myMax x x' = case x < x' of
  True -> x'
  False -> x

myTuple :: a -> b -> (a, b)
myTuple x x' = (x, x')

myTruple :: a -> b -> c -> (a, b, c)
myTruple x x' x'' = (x, x', x'')

myFst :: (a, b) -> a
myFst (x, _) = x

mySnd :: (a, b) -> b
mySnd (_, x) = x

mySwap :: (a, b) -> (b, a)
mySwap (x, x') = (x', x)

myHead :: [a] -> a
myHead (x:_) = x

myTail :: [a] -> [a]
myTail (_:xs) = xs

myLength :: [a] -> Int
myLength [] = 0
myLength (_: xs) = 1 + myLength xs

myNth :: [a] -> Int -> a
myNth [] _ = error "empty list or index too long"
myNth (x:_) 0 = x
myNth (_:xs) y = myNth xs (y - 1)

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake x (y:ys) = (y:myTake (x - 1) ys)

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 ys = ys
myDrop x (_:ys) = myDrop (x-1) ys

myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend xs [] = xs
myAppend (x:xs) ys = (x:myAppend xs ys)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myAppend (myReverse xs) [x]

myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit xs =  myReverse $ myTail $ myReverse xs

myLast :: [a] -> a
myLast [] = error "empty list"
myLast xs =  myHead $ myReverse xs

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] =  []
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip (x:xs) = (myFst x:myFst (myUnzip xs), mySnd x:mySnd (myUnzip xs))

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) = case f x of
    True -> x : myFilter f xs
    False -> myFilter f xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ x [] = x
myFoldl f x (y:ys) =  myFoldl f (f x y) ys

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ x [] = x
myFoldr f x (y:ys) = myFoldr f (f y x) ys

myPartition :: (a -> Bool) -> [a] -> ([a] , [a])
myPartition _ [] = ([], [])
myPartition f (x:xs) =  case f x of
    True -> (x:myFst(myPartition f xs), mySnd(myPartition f xs))
    False -> (myFst(myPartition f xs), x:mySnd(myPartition f xs))

myPartitionQuick :: (a -> a -> Bool) -> a -> [a] -> ([a] , [a])
myPartitionQuick _ _ [] = ([], [])
myPartitionQuick f x' (x:xs) =  case f x x' of
    True -> (x:myFst(myPartitionQuick f x' xs), mySnd(myPartitionQuick f x' xs))
    False -> (myFst(myPartitionQuick f x' xs), x:mySnd(myPartitionQuick f x' xs))

myQuickSort :: (a -> a -> Bool) -> [a] -> [a]
myQuickSort _ [] = []
myQuickSort f (x:xs) = myAppend (myAppend (myQuickSort f (myFst (myPartitionQuick f x xs))) ([x])) (myQuickSort f (mySnd (myPartitionQuick f x xs)))

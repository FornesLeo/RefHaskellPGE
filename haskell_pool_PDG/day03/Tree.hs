data Tree a = Node (Tree a) a (Tree a) | Empty
    deriving Show

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node tree x tree') = Node (fmap f tree) (f x) (fmap f tree')

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node l x r) = foldMap f l `mappend`f x `mappend` foldMap f r

addInTree :: Ord a => a -> Tree a -> Tree a
addInTree x Empty = Node Empty x Empty
addInTree x (Node tree y tree') = case x > y of
    True    -> Node tree y (addInTree x tree')
    False   -> Node (addInTree x tree) y tree'

listToTree :: Ord a => [a] -> Tree a
listToTree = foldr addInTree Empty

-- listToTree :: Ord a => [a] -> Tree a
-- listToTree [] = Empty
-- listToTree (x:xs) = addInTree x $ listToTree xs

treeToList :: Tree a -> [a]
treeToList = foldr (:) []

-- treeToList :: Tree a -> [a]
-- treeToList Empty = []
-- treeToList (Node x y z) = (treeToList x) ++ [y] ++ (treeToList z)

treeSort :: Ord a => [a] -> [a]
treeSort x = treeToList $ listToTree x

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)

addInTree :: Ord a => a -> Tree a -> Tree a
addInTree value Empty = Node Empty value Empty
addInTree value (Node left nodeVal right) | value < nodeVal = Node (addInTree value left) nodeVal right
                                          | otherwise = Node left nodeVal (addInTree value right)

instance Functor Tree where
  fmap = treeFmap

treeFmap :: (a -> b) -> Tree a -> Tree b
treeFmap func Empty = Empty
treeFmap func (Node left nodeVal right) = Node (fmap func left) (func nodeVal) (fmap func right)

listToTree :: Ord a => [a] -> Tree a
listToTree = foldr addInTree Empty

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Node left nodeVal right) = treeToList left ++ nodeVal : treeToList right

treeSort :: Ord a => [a] -> [a]
treeSort a = treeToList (listToTree a)

instance Foldable Tree where
  foldr = treeFoldr

treeFoldr :: (a -> b -> b) -> b -> Tree a -> b
treeFoldr _ init Empty = init
treeFoldr func init (Node left nodeVal right) = foldr func (func nodeVal (foldr func init right)) left

main :: IO ()
main = pure ()
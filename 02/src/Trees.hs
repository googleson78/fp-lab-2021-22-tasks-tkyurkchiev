{-# LANGUAGE InstanceSigs #-}

module Trees where

import Prelude
import Data.Monoid (Sum(..), All(..), Any(..), First(..))

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving Show

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) Empty Empty = True
  (==) (Node a1 l1 r1) (Node a2 l2 r2) = a1 == a2 && l1 == l2 && r1 == r2
  (==) _ _ = False

insertOrdered :: Ord a => a -> Tree a -> Tree a
insertOrdered a Empty = Node a Empty Empty
insertOrdered a (Node b l r)
  | a < b = Node b (insertOrdered a l) r
  | a > b = Node b l (insertOrdered a r)
  | a == b = Node b (insertOrdered a l) r
  | otherwise = Node a l r

listToBST :: Ord a => [a] -> Tree a
listToBST [] = Empty
listToBST xs = foldr insertOrdered Empty xs


--slower version without BotTop
isBST :: Ord a => Tree a -> Bool
isBST Empty = True
isBST (Node a l r) = isBST l && isBST r && all (<= a) (inorder l) && all (> a) (inorder r)
  where inorder Empty = []
        inorder (Node x y z) = inorder y ++ [x] ++ inorder z


findBST :: Ord a => a -> Tree a -> Bool
findBST _ Empty = False
findBST a (Node b l r)
  | a < b = findBST a l
  | a > b = findBST a r
  | a == b = True
  | otherwise = False

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)

foldTree :: Monoid a => Tree a -> a
foldTree Empty = mempty
foldTree (Node a l r) = foldTree l <> a <> foldTree r


foldMapTree :: Monoid b => (a -> b) -> Tree a -> b
foldMapTree _ Empty = mempty
foldMapTree f (Node a l r) = foldMapTree f l <> f a <> foldMapTree f r

sumTree :: Num a => Tree a -> a
sumTree = getSum . foldMapTree Sum

allTree :: (a -> Bool) -> Tree a -> Bool
allTree _ Empty = True
allTree f (Node a l r) = getAll . foldMapTree (All . f) $ Node a l r


treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Node a l r) = treeToList l ++ [a] ++ treeToList r

elemTree :: Eq a => a -> Tree a -> Bool
elemTree a = getAny . foldMapTree (Any . (== a))


onMaybe :: (a -> Bool) -> a -> Maybe a
onMaybe p x = if p x then Just x else Nothing


findPred :: (a -> Bool) -> Tree a -> Maybe a
findPred _ Empty = Nothing
findPred p (Node a l r) = getFirst $ foldMapTree (First . onMaybe p) $ Node a l r

findAll :: (a -> Bool) -> Tree a -> [a]
findAll _ Empty = []
findAll p (Node a l r) = foldMapTree (\x -> [x | p x]) $ Node a l r


maybeAndThen :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeAndThen Nothing _ = Nothing
maybeAndThen (Just x) f = f x

traverseTreeMaybe :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
traverseTreeMaybe _ Empty = Just Empty
traverseTreeMaybe f (Node a l r) = traverseTreeMaybe f l `maybeAndThen` \l' -> f a `maybeAndThen` \a' -> traverseTreeMaybe f r `maybeAndThen`\r' -> Just (Node a' l' r')

data Direction
  = L -- go left
  | R -- go right
  deriving (Show, Eq)

fetch :: [Direction] -> Tree a -> Maybe a
fetch [] (Node a _ _) = Just a
fetch (L:dx) (Node _ l _) = fetch dx l
fetch (R:dx) (Node _ _ r) = fetch dx r
fetch _ _ = Nothing


--paths without mapDirections, could try later with mapDirections if i have time
paths :: Tree a -> [(a, [Direction])]
paths Empty = []
paths (Node a l r) = map (\(x, y) -> (x, L:y)) (paths l) ++ [(a, [])] ++ map (\(x, y) -> (x, R:y)) (paths r)

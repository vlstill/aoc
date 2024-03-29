{-# LANGUAGE TypeApplications, Strict #-}
module T18 where

data Tree = Leaf Int
          | Node Tree Tree
          deriving (Show, Read, Eq)

go :: [Tree] -> Tree
go [x] = x
go (x:y:xs) = go $ reduce (Node x y) : xs

mag (Node l r) = 3 * mag l + 2 * mag r
mag (Leaf x)   = x


data Back = L Tree | R Tree deriving Show
data TZip = TZip { back :: [Back], tree :: Tree } deriving Show

zipper t = TZip [] t

unzipper (TZip [] t) = t
unzipper z           = unzipper (goUp z)

depth :: TZip -> Int
depth = length . back

goL (TZip back (Node l r)) = TZip (R r : back) l
goR (TZip back (Node l r)) = TZip (L l : back) r

onNextL :: (TZip -> TZip) -> TZip -> TZip
onNextL f z@(TZip (L _:_) _) = goR (goUp (onRightmost f (goL (goUp z))))
onNextL f z@(TZip []    _) = z -- do nothing
onNextL f z@(TZip (R _:_) _) = goL (onNextL f (goUp z))

onRightmost :: (TZip -> TZip) -> TZip -> TZip
onRightmost f z@(TZip back (Leaf _)) = f z
onRightmost f z                      = goUp (onRightmost f (goR z))

onNextR :: (TZip -> TZip) -> TZip -> TZip
onNextR f z@(TZip (R _:_) _) = goL (goUp (onLeftmost f (goR (goUp z))))
onNextR f z@(TZip []    _) = z -- do nothing
onNextR f z@(TZip (L _:_) _) = goR (onNextR f (goUp z))

onLeftmost :: (TZip -> TZip) -> TZip -> TZip
onLeftmost f z@(TZip back (Leaf _)) = f z
onLeftmost f z                      = goUp (onLeftmost f (goL z))

goUp (TZip (L l:back) r) = TZip back (Node l r)
goUp (TZip (R r:back) l) = TZip back (Node l r)

set t (TZip back _) = TZip back t

reduce :: Tree -> Tree
reduce t = case red (zipper t) of
              Left t'  -> reduce (unzipper t')
              Right _  -> t
  where
    red z = tryExplode z >> trySplit z

trySplit :: TZip -> Either TZip ()
trySplit z@(TZip _ (Node _ _)) = trySplit (goL z) >> trySplit (goR z)
trySplit z@(TZip _ (Leaf v))
  | v >= 10                    = Left $ split z
  | otherwise                  = Right ()

tryExplode :: TZip -> Either TZip ()
tryExplode z@(TZip _ (Node _ _))
  | depth z >= 4 = Left $ explode z
  | otherwise    = tryExplode (goL z) >> tryExplode (goR z)
tryExplode z@(TZip _ (Leaf _)) = Right ()

explode :: TZip -> TZip
explode z@(TZip _ (Node (Leaf l) (Leaf r))) = set (Leaf 0) $ onNextR (add r) $ onNextL (add l) z

add :: Int -> TZip -> TZip
add val (TZip back (Leaf v)) = TZip back (Leaf (v + val))

split :: TZip -> TZip
split (TZip back (Leaf v)) = TZip back (Node (Leaf (v `div` 2)) (Leaf ((v + 1) `div` 2)))

main = do
    numbers <- fmap (read @Tree) . lines <$> getContents
    print . mag $ go numbers

    print . maximum $ [mag (go [a, b]) | a <- numbers, b <- numbers, a /= b]

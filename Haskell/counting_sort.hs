import qualified Data.Map as M

countingSort :: (Ord a, Show a) => [a] -> [a]
countingSort xs =
  let
    l      = length xs
    zeros  = M.fromList $ zip xs (replicate l 0)
    counts = count xs zeros
    xs'    = M.keys counts -- sorted by definition
  in
    countingSortJoin xs' counts

count :: (Ord a, Show a) => [a] -> M.Map a Int -> M.Map a Int
count xs zeros = foldr inc zeros xs
  where
    inc x counts  = M.insert x ((counts M.! x) + 1) counts

countingSortJoin :: (Ord a) => [a] -> M.Map a Int -> [a]
countingSortJoin xs counts =
  foldr (\x sorted -> rep' x ++ sorted) [] xs
  where
    rep' x = replicate (counts M.! x) x

main :: IO ()
main = (print . reverse . countingSort) list
  where
    list = [1, 8, 7, 9, 4, 12, 19, 12, 100, 1, 7, 3] :: [Integer]

import Data.List as L
import GHC.Float

zero :: Int -> Bool
zero k = k == 0

add :: Int -> Int -> Int
add =
  (\x ->
    (\y ->
      if (zero y) then x else add (succ x) (pred y)
    )
  )

(-+-) :: Int -> Int -> Int
(-+-) = add
infixl 3 -+-

type Point = (Double, Double)

-- atan that works with Double
datan :: Double -> Double
datan yox = float2Double $ atan (double2Float yox)

-- pi with double precision
dpi = pi :: Double

-- angle between point and x-axis
-- ranges from [-pi, pi]
angle :: Point -> Double
angle (x, 0)
  | x <= 0    = dpi
  | otherwise = 0

angle (0, y)
  | y >= 0    = dpi / 2
  | otherwise = -dpi / 2

angle (x, y)
  | x < 0 && y > 0 = dpi - ((datan . abs) $ y / x)
  | x > 0 && y > 0 = datan $ y / x
  | x < 0 && y > 0 = (negate . datan . abs) $ y / x
  | otherwise      = -dpi + (datan $ y / x)

-- Sort points in clockwise direction (Comparator)
-- Starts from 9 and goes through 12, 3, 6 and back to 9
clockwiseSorter :: Point -> Point -> Ordering
clockwiseSorter a b
  | alpha > beta = LT
  | alpha < beta = GT
  | otherwise    = EQ
  where
    (alpha, beta) = (angle a, angle b)

pointsSort :: [Point] -> [Point]
pointsSort = L.sortBy clockwiseSorter

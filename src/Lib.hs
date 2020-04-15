module Lib
  ( encodeLine
  , encodeLines
  ) where

-- ordinal, 'ord' will transform ascii characters to integers
import Data.Char (ord)

type Vector a = [a]
type Matrix a = [Vector a]
-- Useful for handling whole sets of text bodies
type MatrixStack a = [Matrix a]

-- Encode a line into ASCII integral values, or -1 if not present.
encodeLine :: Int -> String -> Vector Int
encodeLine w l = enc ++ replicate nm (-1)
  where
    enc = [ord x | x <- l]
    nm = min 0 (w - length enc)

-- Encode a vector of lines, padding vectors of (-1, -1, ..., -1) if
-- there are fewer lines than the defined height. The end result
-- is a (w x h) matrix of integers.
-- Note, this is the function that allows us to transform plain text data into a matrix.
encodeLines :: Int -> Int -> [String] -> Matrix Int
encodeLines w h ls = enc ++ replicate nm (encodeLine w "")
  where
    enc = map (encodeLine w) ls
    nm = min 0 (h - length enc)

scaleVector :: Floating a => a -> Vector a -> Vector a
scaleVector a = map (a *)

-- [a_1, a_2, ... a_n] -> [(a_1)^2, (a_2)^2, ... (a_n)^2]
squareVector :: Floating a => Vector a -> Vector a
squareVector = map (2.0 **)

-- [a_1, a_2, ... a_n] -> [(a_1)^0.5, (a_2)^0.5, ... (a_n)^0.5]
rootVector :: Floating a => Vector a -> Vector a
rootVector = map (0.5 **)

-- This is just a generic structure for T: (M x M) -> M, where the transformation
-- is built on a binary operation B: (R x R) -> R.
zipMatrixWith :: Floating a => (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
zipMatrixWith z = zipWith (zipWith z)

zeroMatrix :: Floating a => Int -> Int -> Matrix a
zeroMatrix w h = replicate h (replicate w 0)

sumMatrices :: Floating a => Int -> Int -> MatrixStack a -> Matrix a
sumMatrices w h = foldl (zipMatrixWith (+)) (zeroMatrix w h)

scaleMatrix :: Floating a => a -> Matrix a -> Matrix a
scaleMatrix a = map (scaleVector a)

avgScalar :: Floating a => MatrixStack a -> a
-- Don't confuse integral here with the calculus term, it's just talking about any $$ x \in Z $$
avgScalar ms = 1.0 / fromIntegral (length ms)

avgMatrices :: Floating a => Int -> Int -> MatrixStack a -> Matrix a
avgMatrices w h ms = scaleMatrix (avgScalar ms) (sumMatrices w h ms)

-- I plan on using this to create grayscale images to visualize the deviance of data sets, probably
-- not for optimization itself.
stdDevMatrix :: Floating a => Int -> Int -> MatrixStack a -> Matrix a
stdDevMatrix w h ms = map rootVector averaged
  where
    avg = avgMatrices w h ms
    diffs = map (\m -> zipMatrixWith (-) m avg) ms
    squared = map (map squareVector) diffs
    averaged = avgMatrices w h squared

rowVecTimesColVec :: Floating a => Vector a -> Vector a -> a
rowVecTimesColVec v1 v2 = sum (zipWith (*) v1 v2)

matTimesColVec :: Floating a => Matrix a -> Vector a -> Vector a
matTimesColVec mat colVec = foldl (\acc row -> acc ++ [rowVecTimesColVec row colVec]) [] mat

-- Note, I call this a plot, but I'm unsure of how best to visualize it at the moment. The problem is
-- the moment a report matrix exceeds 3 column vectors, we're working in 4th dimensional or
-- greater space. Any advice is appreciated. It should still be usable for linear regression
-- and gradient descent however.
trainingPlot :: Floating a => MatrixStack a -> Vector a -> [Vector a]
trainingPlot matrices transformModel = map (`matTimesColVec` transformModel) matrices

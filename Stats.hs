module Stats (
  range
, mean
, geometricMean
, harmonicMean
, var
, stdev
, absMeanDev
, covar
, euclideanDistance
, normalize
, regressionSlope
, olsIntercept
, linearFitSingleValue
, linearFit
, linearResiduals
, corrCoef
, rSquared
, spearmanRho
, freqDist
, positionPercentile
, linearInterp
, coefVar
, skewness
, sampleSkewness
, gaussianProbaDensity
, standardize
, factorial
) where

{- TODO:
    - Naive Bayes
    - K Nearest Neighbors
    - Covariance Matrix
    - Gaussian Cumulative Probability
    - Student t-distribution Probability Density -}

import Data.List

-- arithmetic range
range :: (Ord a, Fractional a) => [a] -> a
range xs = maximum xs - minimum xs

-- mean: the central tendency of a dataset
mean :: (Fractional a) => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

-- geometric mean: the nth root of the product of n values
geometricMean :: (Floating a) => [a] -> a
geometricMean xs = sqrt $ product xs

-- harmonic mean: reciprocal of the average of the reciprocals
harmonicMean :: (Fractional a) => [a] -> a
harmonicMean xs = length' / (mean $ map (\x -> 1 / x) xs)
    where
        length' = fromIntegral $ length xs

-- variance: the sum of the squared deviations from the mean
var :: (Fractional a) => [a] -> a
var xs = sum (map (\a -> (a - _mean)^2) xs) / (fromIntegral (length xs) - 1)
    where 
        _mean = mean xs

-- standard deviation: the square root of the variance
stdev :: (Floating a) => [a] -> a
stdev xs = sqrt (var xs)

-- Absolute Mean Deviation
absMeanDev :: (Fractional a) => [a] -> a
absMeanDev xs = sum $ map (\x -> abs (x - mean') / length') xs
    where
        mean' = mean xs
        length' = fromIntegral $ length xs

-- Coefficient of Variation
coefVar :: (Floating a) => [a] -> a
coefVar xs = stdev xs / mean xs

-- covariance: the sum of the products of the differences between means from two datasetes
covar :: (Fractional a) => [a] -> [a] -> a
covar xs ys = numerator / denominator
    where
        x_mean = mean xs
        y_mean = mean ys
        numerator = sum (map (\a -> (fst a - x_mean) * (snd a - y_mean)) $ zip xs ys)
        denominator = fromIntegral (length xs) - 1

-- normalize: difference from min over range
normalize :: (Fractional a, Ord a) => [a] -> [a]
normalize xs = map (\z -> (z - _min) / _range) xs
    where
        _max = maximum xs
        _min = minimum xs
        _range = range xs

-- standardize: difference from mean over stdev
standardize :: (Floating a, Fractional a, Ord a) => [a] -> [a]
standardize xs = map (\z -> (z - mean') / stdev') xs
    where
        mean' = mean xs
        stdev' = stdev xs

-- euclidean distance
euclideanDistance :: (Floating a) => [(a, a)] -> a
euclideanDistance pairs = sqrt $ sum $ map (\x -> (snd x - fst x)^2) pairs

-- factorial
factorial :: (Fractional a) => a -> a
factorial x = x * factorial (x - 1)

-- combination
combination :: (Fractional a) => a -> a -> a
combination n x = factorial n / (factorial (n - x) * factorial x)

-- Linear Regression Functions
-------------------------------------------------
-- sum of the products of two datasets
sumOfProducts :: (Num a) => [a] -> [a] -> a
sumOfProducts xs ys = sum (zipWith (*) xs ys)

-- sum of squares of a single dataset
sumOfSquares :: (Num a) => [a] -> a
sumOfSquares xs = sum (map (^2) xs)

{- return the slope of a simple linear regression equation
   : the m in f(x) = mx + b
   : the slope of an equation fitting a straight line through xs and ys -}
regressionSlope :: (Fractional a) => [a] -> [a] -> a
regressionSlope xs ys = numerator / denominator
    where
        numerator = (fromIntegral (length xs) * sumOfProducts xs ys) - sum xs * sum ys
        denominator = (fromIntegral (length xs) * sumOfSquares xs) - sum xs ^ 2

{- return the intercept of a simple linear regression equation
   : the b in f(x) = mx + b 
   : the y-intercept of an equation fitting a straight line through xs and ys -}
olsIntercept :: (Fractional a) => [a] -> [a] -> a
olsIntercept xs ys = (sum ys - regressionSlope xs ys * sum xs) / fromIntegral (length xs)

-- predict a single y value from xs, ys, and an x value with linear regression
linearFitSingleValue :: (Fractional a) => [a] -> [a] -> a -> a
linearFitSingleValue xs ys x = olsIntercept xs ys + regressionSlope xs ys * x

{- return list of predicted y values from the regression equation f(x) = mx + b
    : fit for a straight line that minimizes the sum of the squared deviations to the line -}
linearFit :: (Fractional a) => [a] -> [a] -> [a]
linearFit xs ys = [ slope*x + y | x <- xs ] 
    where
        slope = regressionSlope xs ys
        y = olsIntercept xs ys
            

{- return list of residuals from a linear regression fit 
   : the difference between the predicted values and observed values -}
linearResiduals :: (Fractional a) => [a] -> [a] -> [a]
linearResiduals xs ys = map (\a -> snd a - fst a) (zip (linearFit xs ys) ys)


-- Correlation Functions
-------------------------------------------------
-- correlation coefficent (r)
corrCoef :: (Floating a) => [a] -> [a] -> a
corrCoef xs ys = numerator / denominator
    where
        numerator = fromIntegral (length xs) * sumOfProducts xs ys - sum xs * sum ys
        denominator = sqrt (factor xs) * sqrt (factor ys)
        factor xs' = fromIntegral (length xs') * sum (map (^2) xs') - sum xs' ^ 2

-- cofficient of determination (r^2)
rSquared :: (Floating a) => [a] -> [a] -> a
rSquared xs ys = corrCoef xs ys ^ 2


-- Spearman's Rho Functions
-------------------------------------------------
type RankedList a = [(Int, a)]
type Rank = Int

getRankList :: (Ord a) => [a] -> RankedList a
getRankList x = zip [1..] (sortBy (flip compare) x)

getRankFromRankList :: (Ord a) => a -> RankedList a -> Rank
getRankFromRankList x ((rank, y):xs) 
    | x == y = rank
    | otherwise = getRankFromRankList x xs

rank :: (Ord a) => [a] -> [Rank]
rank li = map (`getRankFromRankList` rankList) li 
    where
        rankList = getRankList li
            
zipRank :: [Rank] -> [Rank] -> [(Rank, Rank)]
zipRank x y = zip (rank x) (rank y)

rankDiff :: [Rank] -> [Rank] -> [Rank]
rankDiff x y = map (\a -> snd a - fst a) $ zipRank x y

-- return the sum of the squared ranked differences 
sumOfSquaredDiffs :: (Ord a) => [a] -> [a] -> Int
sumOfSquaredDiffs xs ys = sum $ map (^2) $ rankDiff (rank xs) (rank ys)
    
-- compute Spearman's Rho Correlation
spearmanRho :: (Ord a, Fractional b) => [a] -> [a] -> b
spearmanRho xs ys = 1 - (numerator / denominator) 
    where
        numerator = fromIntegral $ 6 * sumOfSquaredDiffs xs ys
        n = fromIntegral (length xs)
        denominator = fromIntegral $ n * (n^2 - 1)


-- Distribution Functions
--------------------------------------------------
-- Skewness: measure of symmetry in a distribution
skewness :: (Floating a) => [a] -> a
skewness xs = sum $ map (\x -> (x - mean') ^ 3 / s ^ 3) xs
    where
        mean' = mean xs
        s = stdev xs
 
-- Sample skewness: measure of symmetry in a sample of a distribution
sampleSkewness :: (Floating a) => [a] -> a
sampleSkewness xs = (n / (n - 1) * (n - 2)) * skewness xs
    where
        n = fromIntegral $ length xs

-- Frequency Distribution
freqDist :: (Fractional a, Ord a) => [a] -> [(a, Int)]
freqDist xs = map (\x -> (head x, length x)) $ group $ sort xs

-- Position Percentile
positionPercentile :: (Fractional a) => a -> a -> a
positionPercentile numSamples x = (1 + numSamples) * (x / 100)

-- Linear Interpolation
linearInterp :: (Fractional a) => a -> a -> a -> a -> a
linearInterp x x' y1 y2 = x + (x' - x) * (y2 - y1)
        
-- Gaussian Probability Density Function
-----------------------------------------------
{- given an X value, the mean, and standard devation, 
   return the probability that the X value is within 
   the given normal distribution -}
gaussianProbaDensity :: (Floating a) => a -> a -> a -> a
gaussianProbaDensity x _mean _stdev = (1 / sqrt(two_times_stdev_squared * realToFrac pi)) * (2.7182 ** exponent)
    where 
        two_times_stdev_squared = (2 * _stdev)^2
        exponent = ((x-_mean)^2 / two_times_stdev_squared) * (-1)


-- Probability Functions
----------------------------------------------
{- Joint Probability: the proba of A and B
    - the sum of the probabilities of the outcomes
    - the product of P(A|B) and B  -}
jointProba :: (Fractional a) => a -> a -> a
jointProba condProba b = condProba * b

-- Conditional Probability: the proba of A, given B has occurred P(A|B)
conditionalProba :: (Fractional a, Ord a) => a -> a -> a
conditionalProba jointProba' b 
    | b < 0 = error "B cannot be zero"
    | otherwise = jointProba' / b

-- Addition Rule: the proba of A or B, or both
additionProba :: (Fractional a) => a -> a -> a -> a
additionProba a b jointProba' = a + b - jointProba'

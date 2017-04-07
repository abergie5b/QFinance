module Finance (
  futureValue
, compoundingFV
, continuousCompoundingFV
, effectiveAnnualRate
, periodicRate
, annuityPV
, annuityFV
, presentValue
, compoundingPV
, perpetuity
, growth
, timeWeightedRR
, bankDiscountYield
, holdingPeriodYield
, effectiveAnnualYield
, moneyMarketYield
, sharpeRatio
, cashFlowsFV
, compoundingCashFlowsFV
, capm
, ddm
, nondiverRisk
, sustainableGrowthRate
, assetBeta
, equityBeta
) where

import Stats (mean, stdev, geometricMean)

{- TODO: 
    - Internal (Money-Weighted) Rate of Return
    - Bond Equivalent Yield -}

{- Future Value of a Lump Sum
    - how much is pv worth in x periods at r (discount rate) -}
futureValue :: (Floating a) => a -> a -> a -> a
futureValue pv rate periods = pv * (1 + rate) ** periods

{- Future Value with Quarterly Compounding:
    - pv        = $10000
    - rate      = 0.08
    - n (years) = 2
    - periods   = 4
    > compoundingFV pv rate n periods -> $11,716.59
Future Value with Monthly Compounding:
    - pv        = $1000
    - rate      = 0.06
    - n (years) = 1
    - periods   = 12
    > compoundingFV pv rate n periods -> $1,061.68 -}
compoundingFV :: (Floating a) => a -> a -> a -> a -> a
compoundingFV pv rate n periods = pv * (1 + periodicRate) ** (n * periods) 
    where
        periodicRate = rate / periods

-- Continuous Compounding
continuousCompoundingFV :: (Floating a) => a -> a -> a -> a
continuousCompoundingFV pv rate n = pv * exp 1 ** (rate * n)

-- Effective Annual Interest Rate (solve for the periodic rate)
effectiveAnnualRate :: (Floating a) => a -> a -> a
effectiveAnnualRate periodicRate n = ((1 + periodicRate) ** n) - 1

-- Periodic Rate (annual rate over number of periods)
periodicRate :: (Fractional a) => a -> a -> a
periodicRate rate periods = rate / periods

{- Present Value of an Annuity
    - the current value of a series of annual payments starting from t + 1 -}
annuityPV :: (Floating a) => a -> a -> a -> a
annuityPV notional rate periods = notional * (1 - (1 + rate) ** (-periods)) / rate

{- Future Value of an Annuity
    - the future value of a series of annual payments starting from t + 1 -}
annuityFV :: (Floating a) => a -> a -> a -> a
annuityFV notional rate periods = notional * ((1 + rate) ** periods - 1) / rate

{- Present Value:
    - the current value of a discounted future payment
    - how much do I invest today to make fv at a later date?
    - fv      = $100,000
    - rate    = 0.08
    - periods = 6
    > presentValue fv rate periods -> $63,016.96 -}
presentValue :: (Floating a) => a -> a -> a -> a
presentValue fv rate periods = fv * (1 + rate) ** (-periods)

{- Present Value of Lump Sum with Compounding Interest
    - fv        = $5,000,000
    - rate      = 0.06
    - n (years) = 10
    - periods   = 12
    > compoundingPV fv rate years periods -> $2,748,163.67 -}
compoundingPV :: (Floating a) => a -> a -> a -> a -> a
compoundingPV fv rate n periods = fv * (1 + periodicRate) ** (-n * periods)
    where 
        periodicRate = rate / periods

-- Present Value of a Perpetuity (infinite series of cash flows)
perpetuity :: (Floating a) => a -> a -> a
perpetuity notional rate = notional / rate

-- Growth (Interest) Rate
growth :: (Floating a) => a -> a -> a -> a
growth fv pv periods = (fv / pv) ** (1 / periods) - 1

{- cashFlows 
    - Net Present Value:
    - outflow = -2000000
    - values = [500000, 750000, 1350000]
    - rate = 0.10 
    > cashFlowsFV values rate - 2000000-> $88,655 -}
cashFlowsFV :: (Fractional a) => [a] -> a -> a
cashFlowsFV xs r = sum $ map f $ enumerate xs
    where
        enumerate xs = zip xs [0 ..]
        f (currentValue, index) = currentValue * (1 + r) ^^ index

-- Compounding Cash Flows (Unequal Payments)
compoundingCashFlowsFV :: (Floating a, Enum a) => [a] -> a -> a -> a
compoundingCashFlowsFV xs r n = sum $ map f $ enumerate xs
    where
        enumerate xs = zip xs [0 ..]
        f (currentValue, index) = currentValue * (1 + r / n) ** (index * n)

{- Time Weighted Rate of Return
    - product of the holding period rate of returns less 1
    - dividends should be included into the initial portfolio value
    > let hprrs = [0.20, 0.05, 0.12, -0.10]
    > timeWeightedRR hprrs -> 0.27 -}
timeWeightedRR :: (Floating a) => [a] -> a
timeWeightedRR xs = (foldl (*) 1 $ map (+1) xs) - 1

-- Bank Discount Yield: annualized discount as a percentage of par
bankDiscountYield :: (Floating a) => a -> a -> a -> a
bankDiscountYield price par time = d / par * (360 / time)
    where
        d = par - price
        
-- Holding Period Yield (Total Return): holding period returns for a fixed-income instrument
holdingPeriodYield :: (Floating a) => a -> a -> a -> a
holdingPeriodYield maturityPrice initPrice interest = (maturityPrice - initPrice + interest) / initPrice

-- Effective Annual Yield (EAY)
effectiveAnnualYield :: (Floating a) => a -> a -> a -> a -> a
effectiveAnnualYield maturityPrice initPrice interest time = (1 + hpy) ** (365 / time) - 1
    where
        hpy = holdingPeriodYield maturityPrice initPrice interest
        
-- Money Market Yield (CD Equivalent Yield)
moneyMarketYield :: (Floating a) => a -> a -> a
moneyMarketYield bankDiscountYield time = (360 * bankDiscountYield) / (360 - (time * bankDiscountYield))

-- Money Market Yield (Fixed Income)
moneyMarketYield' :: (Floating a) => a -> a -> a
moneyMarketYield' par price n = ((par - price) / price) * (360 / n)

-- Geometric Mean Returns (Compound Returns)
geometricMeanReturns :: (Floating a) => [a] -> a
geometricMeanReturns xs = geometricMean xs - 1

{- Sharpe Ratio
    - mean' -> average returns of asset
    - riskFreeMean -> average benchmark returns
    - s -> standard deviation of asset -}
sharpeRatio :: (Floating a) => a -> a -> a -> a
sharpeRatio mean' riskFreeMean s = (mean' - riskFreeMean) / s

{- Capital Asset Pricing Model
    - the sum of the risk-free rate of interest and a risk premium 
    - risk premium is beta times the difference between expected market returns and the risk-free rate-}
capm :: (Floating a) => a -> a -> a -> a
capm riskFreeRate beta expectedReturns = riskFreeRate + riskPremium
    where 
        riskPremium = beta * (expectedReturns - riskFreeRate)

{- Dividend-Discount Model
    - the present value of a share's expected future dividends, assuming a constant growth rate
    - forward annual dividend yield is the dividend over price -}
ddm :: (Floating a) => a -> a -> a -> a
ddm dividend price r = fwdAnnualYield + r
    where 
        fwdAnnualYield = dividend / price

-- Earnings Retention Rate
earningsRetentionR :: (Floating a) => a -> a -> a
earningsRetentionR dividend eps = (1 - dividend / eps)

-- Sustainable Growth Rate
sustainableGrowthRate :: (Floating a) => a -> a -> a -> a
sustainableGrowthRate dividend eps roe = earningsRetentionR' * roe
    where 
        earningsRetentionR' = earningsRetentionR dividend eps
        
-- Non-diversifiable Risk
nondiverRisk :: (Floating a) => a -> a -> a
nondiverRisk tax debtToEquity = 1 + ((1 - tax) * debtToEquity)

-- Asset Beta: unlevered beta, the business risk of assets
assetBeta :: (Floating a) => a -> a -> a -> a
assetBeta equityBeta tax debtToEquity = equityBeta * (1 / nondiverRisk tax debtToEquity)

-- Equity Beta: levered beta
equityBeta :: (Floating a) => a -> a -> a -> a
equityBeta assetBeta tax debtToEquity = assetBeta * nondiverRisk tax debtToEquity

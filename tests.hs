import Test.HUnit
import Stats

main = runTestTT tests

olsXs = [95, 85, 80, 70, 60]
olsYs = [85, 95, 70, 65, 70]

rhoxs = [56, 75, 45, 71, 62, 64, 58, 80, 76, 61]
rhoys = [66, 70, 40, 60, 65, 56, 59, 77, 67, 63]

eucDistxs = [(0, 7), (3, 6), (4, 3), (5, -1)]

meanTest            = TestCase (assertEqual "for mean," 78 (mean olsXs))
varTest             = TestCase (assertEqual "for var," 182.5 (var olsXs))
stdevTest           = TestCase (assertEqual "for stdev," 13.509 (stdev olsXs))
olsInterceptTest    = TestCase (assertEqual "for olsIntercept," 26.768 (olsIntercept olsXs olsYs))
olsSlope            = TestCase (assertEqual "for olsSlope," 0.644 (regressionSlope olsXs olsYs))
correlationCoef     = TestCase (assertEqual "for corrCoef," 0.693 (corrCoef olsXs olsYs))
pearsonR            = TestCase (assertEqual "for rSquared," 0.48 (rSquared olsXs olsYs))
covarTest           = TestCase (assertEqual "for covar," 117.5 (covar olsXs olsYs))
spearmanTest        = TestCase (assertEqual "for spearman," 0.67 (spearmanRho rhoxs rhoys))
euclideanDistTest   = TestCase (assertEqual "for euclidean distance," 9.747 (euclideanDistance eucDistxs))

tests = TestList [TestLabel "meanTest" meanTest, 
                  TestLabel "varTest" varTest,
                  TestLabel "stdevTest" stdevTest,
                  TestLabel "olsIntercept" olsInterceptTest, 
                  TestLabel "olsSlope" olsSlope, 
                  TestLabel "correlationCoef" correlationCoef,
                  TestLabel "pearsonR" pearsonR,
                  TestLabel "covar" covarTest,
                  TestLabel "spearman" spearmanTest,
                  TestLabel "euclideanDistance" euclideanDistTest]

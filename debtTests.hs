import Debt
import Test.QuickCheck
import Data.Map
import Text.Printf
 
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

--Sum of all balances is within epsilon of 0 
prop_balanceSum s = (sumBalances $ removeSelfDebt s) < 0.00001

prop_consolidateSum s = (sumBalances $ consolidateDebts $ removeSelfDebt s) < 0.00001

sumBalances = sum . snd . unzip . toList . getBalances
 
tests  = [("balance.sum/0", quickCheck prop_balanceSum)
         ,("prop_consolidate.sum/0", quickCheck prop_consolidateSum)
         ]

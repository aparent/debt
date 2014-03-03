module Debt ( DebtList, getBalances, consolidateDebts, removeSelfDebt ) where

import Data.Word
import Data.Map as M
import Data.Maybe
import Data.List as L

import System.Random


type DebtList = [((Word,Word), Double)]

getBalances :: DebtList -> M.Map Word Double
getBalances = L.foldl' applyDebt empty 
  where applyDebt balances ((a,b),amount) = updateB $ updateA balances  
          where updateA = M.alter (updateBalance amount) a
                updateB = M.alter (updateBalance $ negate amount) b
                updateBalance n (Just currentBalance)  = Just $ currentBalance + n
                updateBalance n  Nothing               = Just n

consolidateDebts :: DebtList -> DebtList
consolidateDebts = catMaybes . L.map consolidatePairs . groupDebtPairs 
  where consolidatePairs (a:[])                             = Just a
        consolidatePairs (((a0,a1),aVal):((b0,b1),bVal):[]) = Just ((a0,a1),aVal-bVal)
        consolidatePairs _                                  = Nothing  --Should be an error?  should never happen
        groupDebtPairs = L.groupBy (\a b -> compEq $ debtOrd a b) . L.sortBy debtOrd
          where debtOrd  ((a0,a1),_) ((b0,b1),_) = compare ((max a0 a1), (min a0 a1)) ((max b0 b1), (min b0 b1)) 
                compEq EQ = True
                compEq _  = False

removeSelfDebt :: DebtList -> DebtList 
removeSelfDebt = L.filter (\((x1,x2),_) -> x1 /= x2)

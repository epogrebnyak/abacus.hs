module Main (main) where

import Test.HUnit
import Abacus
import Data.Map as Map
import Control.Monad.State (State, get, put, runState, execState, evalState, modify)
import GHC.Windows (getErrorMessage)


-- FIXME - must reuse from package
account :: T5 -> Name -> [Name] -> [Primitive]
account t name contraNames = PAdd t name : Prelude.map (POffset name) contraNames

-- Chart where contra accounts not listed alphabetically 
chartSample :: ChartMap
chartSample = fromChartItems (account Income "sales" ["voids", "refunds"] ++ -- ... real names
                              account Expense "cogs" ["sgoc", "abc"]) -- ... fictional names

-- End-to-end test for chart creation
testWhichSide :: Test
testWhichSide = TestCase $ do
    assertEqual "Refunds is a debit account" (Just Debit) (whichSide "refunds" chartSample)

-- Contra account names are alphabetically sorted 
testContras :: Test
testContras = TestCase $ do
    assertEqual "Return contra accounts for cogs" ["abc", "sgoc"] (contras chartSample "cogs")

-- Code under test is closingPairs :: ChartMap -> Name -> [(Name, Name)]
testClosingPairs :: Test
testClosingPairs = TestCase $ do
    let ref =  [("abc","cogs"), ("sgoc","cogs"), -- close contra expense
                ("cogs","re"), -- close expense to retained earnings 
                ("refunds","sales"),("voids","sales"), -- close contra income
                ("sales","re")] -- close income to retained earnings 
    assertEqual "List closing pairs for chartSample" ref (closingPairs chartSample "re")

-- Test Experimental.hs (generated from code)

testPairState :: Test
testPairState = TestCase $ do
    let h = Map.fromList [("a", debitAccount 10), ("b", creditAccount 0)]
    let (k, g) = runState (pairState (Pair "a" "b")) h
    let res = Map.fromList [("a",TAccount Debit 10 10),("b",TAccount Credit 10 0)]
    assertEqual "pairState should return Right DoubleEntry' b a 10" (Right (DoubleEntry' "b" "a" 10)) k
    assertEqual "pairState should update the ledger" g res

sampleAccMap :: Map String TAccount
sampleAccMap = Map.fromList
    [ ("re", creditAccount 0),
      ("sales", creditAccount 10),
      ("refunds", debitAccount 2),
      ("expense", debitAccount 3)
    ]

getRe :: Map String TAccount -> Amount
getRe accMap = accountBalance (accMap Map.! "re")

testCloseThem :: Test
testCloseThem = TestCase $ do
    let pairs = [Pair "refunds" "sales", Pair "sales" "re"]
    let closedMap = closeThem pairs sampleAccMap
    assertEqual "resulting balance for re is 8" 8 (getRe closedMap)

testCloseThemNonExistent :: Test
testCloseThemNonExistent = TestCase $ do
    let unhappyPairs = [Pair "doesNotExist" "expense", Pair "expense" "re"]
    let closedMap = closeThem unhappyPairs sampleAccMap
    assertEqual "handles non-existent pair" (-3) (getRe closedMap)

testCollectTransferEntries :: Test
testCollectTransferEntries = TestCase $ do
    let unhappyPairs = [Pair "doesNotExist" "expense", Pair "expense" "re"]
    let results = collectTransferEntries unhappyPairs sampleAccMap
    let expected = [Left ["doesNotExist"], Right (DoubleEntry' "re" "expense" 3)]
    assertEqual "magically collects 2 results"  expected results

testMorePairs :: Test
testMorePairs = TestCase $ do
    let morePairs = [Pair "refunds" "sales", Pair "sales" "re"] ++ 
                    [Pair "doesNotExist" "expense", Pair "expense" "re"]
    let closedMap = closeThem morePairs sampleAccMap
    assertEqual "resulting balance for re is 5" 5 (getRe closedMap)

-- Main function to run the test
main :: IO ()
main = do
    _ <- runTestTT testWhichSide
    _ <- runTestTT testContras
    _ <- runTestTT testClosingPairs
    _ <- runTestTT testPairState
    _ <- runTestTT testCloseThem
    _ <- runTestTT testCloseThemNonExistent
    _ <- runTestTT testCollectTransferEntries
    _ <- runTestTT testMorePairs
    return ()

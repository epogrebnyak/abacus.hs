module Experimental where

import Chart (closingPairs)
import Control.Monad.State
  ( State,
    evalState,
    execState,
    get,
    modify,
    put
   )
import qualified Data.Map as Map
import Ledger (alter)
import Types
    ( ChartMap,
      AccountMap,
      TAccount(..),
      Side(..),
      Name,
      Amount,
      accountBalance )

data Pair = Pair Name Name deriving (Show, Eq)
data DoubleEntry' = DoubleEntry' Name Name Amount deriving (Show, Eq)
data SingleEntry' = S Side Name Amount deriving (Show, Eq)

-- Helper Functions
debitAccount :: Amount -> TAccount
debitAccount b = TAccount Debit b 0

creditAccount :: Amount -> TAccount
creditAccount b = TAccount Credit 0 b

debit :: Name -> Amount -> SingleEntry'
debit = S Debit

credit :: Name -> Amount -> SingleEntry'
credit = S Credit

-- Convert ChartMap to a list of Pairs
toPairs :: ChartMap -> Name -> [Pair]
toPairs chartMap sumName = map (uncurry Pair) (closingPairs chartMap sumName)

-- Convert DoubleEntry' to a pair of SingleEntry'
toSingles :: DoubleEntry' -> (SingleEntry', SingleEntry')
toSingles (DoubleEntry' d c a) = (debit d a, credit c a)

-- Create a DoubleEntry' from account names and TAccount
mkTransferEntry :: Name -> Name -> TAccount -> DoubleEntry'
mkTransferEntry fromName toName t@(TAccount side _ _) =
  let b = accountBalance t
   in case side of
        Debit -> DoubleEntry' toName fromName b
        Credit -> DoubleEntry' fromName toName b

-- Apply SingleEntry' to the ledger
singleState :: SingleEntry' -> State AccountMap (Either Name ())
singleState (S side name amount) = do
  accMap <- get
  case Map.lookup name accMap of
    Just tAccount -> do
      put $ Map.insert name (alter side amount tAccount) accMap
      return $ Right ()
    Nothing -> return $ Left name

-- Check if two account names are in the ledger
acceptNames :: Name -> Name -> AccountMap -> Either [Name] DoubleEntry'
acceptNames fromName toName accMap =
  let get' = flip Map.lookup accMap in
  case (get' fromName , get' toName) of
    (Just _, Just _)   -> Right $ mkTransferEntry fromName toName (accMap Map.! fromName)  
    (Nothing, Just _)  -> Left [fromName]
    (Just _, Nothing)  -> Left [toName]
    (Nothing, Nothing) -> Left [fromName, toName]

-- Modify the ledger using transfer balance command
pairState :: Pair -> State AccountMap (Either [Name] DoubleEntry')
pairState (Pair fromName toName) = do
  accMap <- get
  case acceptNames fromName toName accMap of
    Right doubleEntry -> do
      let (a, b) = toSingles doubleEntry
      modify (execState (singleState a >> singleState b))
      return $ Right doubleEntry
    Left err -> return $ Left err

-- Close multiple pairs in the ledger
closeThem :: [Pair] -> AccountMap -> AccountMap
closeThem pairs = execState (mapM_ pairState pairs)

-- Collect transfer entries for a list of pairs
collectTransferEntries :: [Pair] -> AccountMap -> [Either [Name] DoubleEntry']
collectTransferEntries pairs accMap = evalState (mapM pairState pairs) accMap

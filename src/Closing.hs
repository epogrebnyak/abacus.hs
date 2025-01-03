module Closing where 

import Ledger 
import Chart (closingPairs, whichSide)
import Types
import qualified Data.Map as Map
import Control.Monad.State (State, get, put, runState)
import Data.Maybe (catMaybes)

data Pair = Pair Name Name
data DoubleEntry' = DoubleEntry' Name Name Amount deriving Show

-- Closing pairs changed to Transfer actions 
anyClose :: ChartMap -> Name -> [Action]
anyClose chartMap accName = map (uncurry Transfer) (closingPairs chartMap accName)

-- Close ledger to an equity account only
closeActions :: ChartMap -> Name -> Either Error [Action]
closeActions chartMap accName =
    case Map.lookup accName chartMap of
        Just (Regular Equity) -> Right $ anyClose chartMap accName
        Just _                -> Left $ NotEquity accName
        Nothing               -> Left $ NotFound accName

------------------------------------------------------------------------------

toPairs :: ChartMap -> Name -> [Pair]
toPairs chartMap sumName = map (uncurry Pair) (closingPairs chartMap sumName)

mkTransferEntry :: Name -> Name -> TAccount -> DoubleEntry'
mkTransferEntry fromName toName t@(TAccount side _ _) =
    let b = accountBalance t in     
    case side of
        Debit -> DoubleEntry' toName fromName b
        Credit -> DoubleEntry' fromName toName b

-- toEntry :: AccountMap -> Pair -> Either Error DoubleEntry'
-- toEntry accMap (Pair fromName toName) 
--     | not (includes fromName) = Left $ NotFound fromName
--     | not (includes toName) = Left $ NotFound toName
--     | otherwise =  Right $ mkTransferEntry fromName toName (accMap Map.! fromName) 
--     where 
--         includes n = Map.member n accMap

data SingleEntry' = S Side Name Amount
debit = S Debit
credit = S Credit

toSingles :: DoubleEntry' -> [SingleEntry']
toSingles (DoubleEntry' d c a) = [debit d a, credit c a]

-- Accept a single entry into the Ledger
applySingle' :: SingleEntry' -> AccountMap -> Either Error AccountMap
applySingle' s accountMap = 
    let (e, accountMap') = runState (singleState s) accountMap in
    case e of
       Nothing -> Right accountMap'
       Just e -> Left e 

singleState :: SingleEntry' -> State AccountMap (Maybe Error)
singleState (S side name amount) = do
    accMap <- get
    case Map.lookup name accMap of
        Just tAccount -> do
            put $ Map.insert name (alter side amount tAccount) accMap
            return Nothing
        Nothing -> return $ Just (NotFound name)

pairState :: Pair -> State AccountMap (Either Error DoubleEntry')
pairState pair@(Pair fromName toName) = do
    accMap <- get
    case Map.lookup fromName accMap of
        Just tAccount -> do
            let doubleEntry = mkTransferEntry fromName toName tAccount
            results <- mapM singleState (toSingles doubleEntry)
            -- what is the type of results?
            -- are we updating the state itself?
            case catMaybes results of
                [] -> return $ Right doubleEntry
                (err:_) -> return $ Left err
        Nothing -> return $ Left (NotFound fromName)


debitAccount b = TAccount Debit b 0 
creditAccount b = TAccount Credit 0 b

h :: AccountMap
h = Map.fromList [("a", debitAccount 10), ("b", creditAccount 12)]

f :: (Either Error DoubleEntry', AccountMap)
f = runState (pairState (Pair "a" "b")) h

g :: Amount
g = let (a, s) = f in accountBalance (s Map.! "b")
-- g equals 2

k = let (a, s) = f in a
-- Right (DoubleEntry' "b" "a" 10)
module Experimental where

import Ledger
import Chart (closingPairs)
import Types
import qualified Data.Map as Map
import Control.Monad.State (State, get, put, runState, execState, modify)

data Pair = Pair Name Name
data DoubleEntry' = DoubleEntry' Name Name Amount deriving Show

toPairs :: ChartMap -> Name -> [Pair]
toPairs chartMap sumName = map (uncurry Pair) (closingPairs chartMap sumName)

mkTransferEntry :: Name -> Name -> TAccount -> DoubleEntry'
mkTransferEntry fromName toName t@(TAccount side _ _) =
    let b = accountBalance t in
    case side of
        Debit -> DoubleEntry' toName fromName b
        Credit -> DoubleEntry' fromName toName b

data SingleEntry' = S Side Name Amount
debit :: Name -> Amount -> SingleEntry'
debit = S Debit
credit :: Name -> Amount -> SingleEntry'
credit = S Credit

toSingles :: DoubleEntry' -> (SingleEntry', SingleEntry')
toSingles (DoubleEntry' d c a) = (debit d a, credit c a)

-- Accept a single entry into the Ledger
applySingle' :: SingleEntry' -> AccountMap -> Either [Name] AccountMap
applySingle' s accountMap =
    let (err, accountMap') = runState (singleState s) accountMap in
    case err of
       Right _ -> Right accountMap'
       Left e' -> Left [e']

singleState :: SingleEntry' -> State AccountMap (Either Name ())
singleState (S side name amount) = do
    accMap <- get
    case Map.lookup name accMap of
        Just tAccount -> do
            put $ Map.insert name (alter side amount tAccount) accMap
            return $ Right ()
        Nothing -> return $ Left name

acceptNames :: Name -> Name -> AccountMap -> Either [Name] DoubleEntry'
acceptNames fromName toName accMap = let t = (accMap `includes`) in
    case (t fromName, t toName) of
        (True, True) -> Right $ mkTransferEntry fromName toName (accMap Map.! fromName)
        (False, True) -> Left [fromName] 
        (True, False) -> Left [toName]
        (False, False) -> Left [fromName, toName]

pairState :: Pair -> State AccountMap (Either [Name] DoubleEntry')
pairState (Pair fromName toName) = do
    accMap <- get
    case acceptNames fromName toName accMap of
        -- lifting to Either, maybe shorter version exists
        Right doubleEntry -> do
            let (a, b) = toSingles doubleEntry
            modify (execState (singleState a >> singleState b))
            return $ Right doubleEntry
        -- ... as this seems redundant    
        Left err -> return $ Left err   

debitAccount :: Amount -> TAccount
debitAccount b = TAccount Debit b 0

creditAccount :: Amount -> TAccount
creditAccount b = TAccount Credit 0 b

h :: AccountMap
h = Map.fromList [("a", debitAccount 10), ("b", creditAccount 0)]

f :: (Either [Name] DoubleEntry', AccountMap)
f = runState (pairState (Pair "a" "b")) h

g :: AccountMap
g = snd f

k :: Either [Name] DoubleEntry'
k = fst f
-- Right (DoubleEntry' "b" "a" 10)

am :: AccountMap
am = Map.fromList [("re", creditAccount 0),
                   ("sales", creditAccount 10),
                   ("refunds", debitAccount 2)]

ps :: [Pair]
ps = [Pair "refunds" "sales", Pair "sales" "re"]

closer :: [Pair] -> AccountMap -> AccountMap
closer pairs = execState (mapM_ pairState pairs)

j :: AccountMap
j = closer ps am

repl :: IO ()
repl = do
    print h
    print g
    print k

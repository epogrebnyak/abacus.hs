module Ledger where

import qualified Data.Map as Map
import Data.Maybe ( isNothing )
import Control.Monad (foldM)
import Control.Monad.State (State, gets, put)

import Chart (emptyAccount, dispatchMany, closingPairs, whichSide)
import Types

-- Create AccountMap from ChartMap using do notation (similar to list comprehension)
toAccountMap :: ChartMap -> AccountMap
toAccountMap chartMap = Map.fromList $ do
    name <- Map.keys chartMap
    Just side <- [whichSide name chartMap]
    return (name, emptyAccount side)

-- Create Ledger
fromChartMap :: ChartMap -> Ledger
fromChartMap chartMap = Ledger chartMap (toAccountMap chartMap) []

-- Create empty Ledger
emptyLedger :: Ledger
emptyLedger = fromChartMap Map.empty

-- Check if a list of single entries is balanced
isBalanced :: [SingleEntry] -> Bool
isBalanced posts = let f = sideSum posts in f Debit == f Credit

-- Sum amounts of all entries of a given side (debit or credit)
sideSum :: [SingleEntry] -> Side -> Amount
sideSum posts side = sum [a | Single s _ a <- posts, s == side]

-- Post single entry to t-account.
alter :: Side -> Amount -> TAccount -> TAccount
alter Debit amount (TAccount side d c)  = TAccount side (d+amount) c
alter Credit amount (TAccount side d c) = TAccount side d (c+amount)

-- Apply a single entry to the AccountMap
applySingle :: SingleEntry -> AccountMap -> Either Error AccountMap
applySingle (Single side name amount) accountMap = case Map.lookup name accountMap of
    Just tAccount -> Right $ Map.insert name (alter side amount tAccount) accountMap
    Nothing -> Left $ NotFound name

-- Accept a single entry into the Ledger
acceptSingle :: Ledger -> SingleEntry -> Either Error Ledger
acceptSingle (Ledger chartMap accountMap names) s@(Single _ name _) =
    if name `elem` names then Left (Dropped name) else 
    case applySingle s accountMap of
        Left err -> Left err
        Right accountMap' -> Right $ Ledger chartMap accountMap' names

-- Accept multiple entries into the Ledger, entries must be balanced
acceptMany :: [SingleEntry] -> Ledger -> Either Error Ledger
acceptMany posts ledger
    | isBalanced posts = acceptUnbalanced posts ledger
    | otherwise = Left $ NotBalanced posts

-- Accept unbalanced entries into the Ledger
acceptUnbalanced :: [SingleEntry] -> Ledger -> Either Error Ledger
acceptUnbalanced posts ledger = foldM acceptSingle ledger posts

-- Close ledger to an equity account
closeActions :: ChartMap -> Name -> Either Error [Action]
closeActions chartMap accName =
    case Map.lookup accName chartMap of
        Just (Regular Equity) -> Right $ anyClose chartMap accName
        Just _                -> Left $ NotEquity accName
        _                     -> Left $ NotFound accName

-- Closing actions 
anyClose :: ChartMap -> Name -> [Action]
anyClose chartMap accName = concat [ 
    [Transfer fromName toName, Deactivate fromName] 
    | (fromName, toName) <- closingPairs chartMap accName]

transferEntry :: Name -> Name -> TAccount -> Entry
transferEntry fromName toName (TAccount side a b) =
    case side of
        Debit -> DoubleEntry toName fromName (a-b)
        Credit -> DoubleEntry fromName toName (b-a)

-- Process ledger actions
process :: Ledger -> [Action] -> Either Error Ledger
process ledger [] = Right ledger
process ledger (l:ls) = do
    ledger' <- run ledger l
    process ledger' ls

toBalanced :: Entry -> Entry
toBalanced (DoubleEntry d c a) = BalancedEntry [Single Debit d a, Single Credit c a]
toBalanced b = b

updateAdd :: ChartMap -> AccountMap -> T5 -> Name -> (ChartMap, AccountMap) 
updateAdd chartMap accMap t name = 
    let chartMap' = Map.insert name (Regular t) chartMap
        accMap' = case whichSide name chartMap' of
            Just side -> Map.insert name (emptyAccount side) accMap
            Nothing -> accMap
    in (chartMap', accMap')

updateOffset :: ChartMap -> AccountMap -> Name -> Name -> (ChartMap, AccountMap)
updateOffset chartMap accMap name contraName = 
    let chartMap' = Map.insert contraName (Contra name) chartMap
        accMap' = case whichSide contraName chartMap' of
            Just side -> Map.insert contraName (emptyAccount side) accMap
            Nothing -> accMap
    in (chartMap', accMap')

data Book = Book {chartM :: ChartMap, ledgerM :: AccountMap, copyM :: Maybe AccountMap}

emptyBook :: Book
emptyBook = Book Map.empty Map.empty Nothing  

isRegular :: Role -> Bool
isRegular (Regular _) = True
isRegular _ = False

allowOffset :: ChartMap -> Name -> Name -> Error
allowOffset chartMap name contraName  
   | isNothing a = NotFound name
   | chartMap `includes` contraName = AlreadyExists contraName
   | (isRegular <$> a) /= Just True = NotRegular name
   | otherwise = OK
   where a = Map.lookup name chartMap

update :: Primitive -> State Book Error  
update p = do
    chart <- gets chartM
    ledger <- gets ledgerM
    copy <- gets copyM 
    case p of
        PAdd t name -> 
            case Map.lookup name chart of
                Just _ -> return (AlreadyExists name)
                Nothing -> do
                    let (chart', ledger') = updateAdd chart ledger t name
                    put $ Book chart' ledger' copy
                    return OK
        POffset name contraName ->
            case allowOffset chart name contraName of
                OK -> do
                  let (chart', ledger') = updateOffset chart ledger name contraName
                  put $ Book chart' ledger' copy
                  return OK
                e -> return e  
        PPost (Single side name amount) -> 
            if not (ledger `includes` name) then return (NotFound name) 
            else do
                let tAccount' = alter side amount (ledger Map.! name)
                let ledger' = Map.insert name tAccount' ledger
                put $ Book chart ledger' copy
                return OK
        PDrop name ->    
            if not (ledger `includes` name) then return (NotFound name) 
            else do
                let ledger' = Map.delete name ledger
                put $ Book chart ledger' copy
                return OK
        PCopy -> do 
            put $ Book chart ledger (Just ledger)
            return OK            

data TransactionError = NotBalanced' [SingleEntry] | NotFound' Name

makeTransfer :: Name -> Name -> AccountMap -> Either TransactionError TAccount  
makeTransfer fromName toName accMap 
   | not (accMap `includes` fromName) = Left (NotFound' fromName)
   | not (accMap `includes` toName) = Left (NotFound' toName)
   | otherwise = Right (accMap Map.! fromName) 


dec :: Action -> State Book (Either TransactionError [Primitive])
dec (Use chartAction) = return $ Right (toPrimitives chartAction) 
dec (Deactivate name) = return $ Right [PDrop name]
dec (Post prose de@(DoubleEntry {})) = dec $ Post prose (toBalanced de)
dec (Post _ (BalancedEntry singles)) = return $ 
    if isBalanced singles 
    then Right (PPost <$> singles) 
    else Left (NotBalanced' singles) 
dec (Transfer fromName toName) = do
    accMap <- gets ledgerM
    case makeTransfer fromName toName accMap of 
        Left e -> return $ Left e
        Right tAccount -> dec $ Post "Transfer entry" (transferEntry fromName toName tAccount)
dec (Close accName) = do
    ledger <- gets ledgerM
    chart <- gets chartM
    if ledger `includes` accName then do
        let actions = anyClose chart accName
        results <- mapM dec actions
        case sequence results of
            Left err -> return $ Left err
            Right primitives -> return $ Right (concat primitives)
    else return $ Left (NotFound' accName)
dec _  = undefined

-- Run a single ledger action
run :: Ledger -> Action -> Either Error Ledger
run (Ledger chartMap _ names) (Use chartAction) = 
    Right $ Ledger chartMap' accountMap' names
    where
        prims = toPrimitives chartAction 
        chartMap' = dispatchMany chartMap prims
        accountMap' = toAccountMap chartMap'
run ledger (Post prose d@(DoubleEntry {})) = run ledger $ Post prose (toBalanced d)
run ledger (Post _ (BalancedEntry singles)) = acceptMany singles ledger
run ledger (Close accName) = do
    actions <- closeActions (_chartMap ledger) accName
    foldM run ledger actions
run ledger (Transfer fromName toName) =
    case Map.lookup fromName (accounts ledger) of
        Just tAccount -> run ledger $ Post "Transfer entry" (transferEntry fromName toName tAccount)
        Nothing -> Left $ NotFound fromName
run (Ledger cm am ds) (Deactivate name) = Right $ Ledger cm am (name:ds)
run ledger (End _) = Right ledger  -- assuming Finish does not change the ledger
run ledger _ = Right ledger  -- other items

-- update :: Action -> State Ledger (Maybe Error) 

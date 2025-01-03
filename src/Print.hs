module Print where 

import Types
import Ledger()
import qualified Data.Map as Map

explain :: Error -> String
explain (NotFound name)      = "Account not found: " ++ name
explain (NotUnique name)     = "Duplicate account name: " ++ name
explain (NotEquity name)     = "Must be an equity account: " ++ name
explain (Dropped name)       = "Account deactivated: " ++ name
explain (NotRegular name)    = "Not regular account: " ++ name
explain (AlreadyExists name) = "Account already exists: " ++ name
explain (NotBalanced posts)  = "Entry not balanced: " ++ show posts -- may use sideSum
explain OK = "No errors"

-- Print account names and balances by lines
printAccountBalances :: Ledger -> IO ()
printAccountBalances ledger = mapM_ putStrLn (accountStrings ledger)
  where
    accountStrings :: Ledger -> [String]
    accountStrings (Ledger _ accountMap _) = do
        (name, tAccount) <- Map.toList accountMap
        return $ name ++ ": " ++ show (accountBalance tAccount)

-- Diagnose ledger for errors or print accounts
diagnose :: Either Error Ledger -> IO ()
diagnose (Left e) = putStrLn $ explain e
diagnose (Right ledger) = printAccountBalances ledger

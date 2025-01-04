module Types where

import qualified Data.Map as Map

type Amount = Int  -- This could be Decimal E2
type Name = String
data Side = Debit | Credit deriving (Show, Eq)
data TAccount = TAccount Side Amount Amount deriving (Show, Eq)

accountBalance :: TAccount -> Amount
accountBalance (TAccount Debit a b) = a - b
accountBalance (TAccount Credit a b) = b - a

type AccountMap = Map.Map Name TAccount

includes :: Ord a => Map.Map a b -> a -> Bool
someMap `includes` name = Map.member name someMap    

-- Account Types
data T5 = Asset | Expense | Equity | Liability | Income deriving (Show, Eq)

-- Chart of Accounts
data Role = Regular T5 | Contra Name deriving (Show, Eq)
type ChartMap = Map.Map Name Role

-- Single Entry
data SingleEntry = Single Side Name Amount deriving Show

-- Error Handling
data Error =  NotFound Name 
            | NotUnique Name 
            | NotEquity Name 
            | NotRegular Name
            | Dropped Name -- will go
            | AlreadyExists Name
            | NotBalanced [SingleEntry] 
            | OK
            deriving Show

-- Ledger
type Balances = Map.Map Name Amount
data Ledger = Ledger {
    _chartMap :: ChartMap,
    accounts :: AccountMap,
    deactivated :: [Name] } deriving Show

-- Accounting Cycle Stages
data Activity = Opening | Business | Adjustment | Closing | PostClose deriving Show

-- Chart Actions
data ChartAction = Account T5 Name
                 | Accounts T5 [Name]
                 | Account' T5 Name [Name]
                 | Offset Name [Name]
                 deriving Show

toPrimitives :: ChartAction -> [Primitive]
toPrimitives (Account  t n)    = [PAdd t n] 
toPrimitives (Accounts t ns)   = map (PAdd t) ns
toPrimitives (Account' t n ns) = PAdd t n : toPrimitives (Offset n ns)
toPrimitives (Offset name ns)  = [POffset name contra | contra <- ns]      

data Entry = DoubleEntry Name Name Amount | BalancedEntry [SingleEntry] deriving Show

-- Ledger actions
data Action =   Use ChartAction
              | Post String Entry 
              | Close Name
              | Transfer Name Name
              | Deactivate Name
              | End Activity
              | Unsafe [Primitive]
              deriving Show

-- Primitives
data Primitive = PAdd T5 Name
               | POffset Name Name
               | PPost SingleEntry
               | PDrop Name  -- same as Deactivate
               | PCopy       -- copy ledger before closing accounts 
               deriving Show

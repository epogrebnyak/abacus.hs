module Abacus (module Types, 
               module Chart, 
               module Ledger, 
               module Print,
               module Closing,
               module Action,
               exampleStream,
               playWithThisChart,               
               playWithThisLedger               
               ) where

import Types
import Chart
import Ledger
import Print
import Closing
import Action

-- Move to tests  
playWithThisChart :: ChartMap
playWithThisChart = fromChartItems $ [
            PAdd Asset "cash", 
            PAdd Equity "equity",
            PAdd Equity "re",
            PAdd Expense "salary",
            PAdd Income "sales", 
            POffset "sales" "refunds"
        ]

playWithThisLedger :: Ledger
playWithThisLedger = fromChartMap playWithThisChart

assets :: [Name] -> Action
assets ns = Use $ Accounts Asset ns

capital :: [Name] -> Action
capital ns = Use $ Accounts Equity ns

liabilities :: [Name] -> Action
liabilities ns = Use $ Accounts Liability ns

incomes :: [Name] -> Action
incomes ns = Use $ Accounts Income ns

expenses :: [Name] -> Action
expenses ns = Use $ Accounts Expense ns

offsets :: Name -> [Name] -> Action
offsets n ns = Use $ Offset n ns
 
offset :: Name -> Name -> Action
offset name contra = Use $ Offset name [contra]

-- "Shareholder investment" |$ 1000 "cash" "eq",

double :: String -> Name -> Name -> Amount -> Action
double comment d c a = Post comment $ DoubleEntry d c a

multiple :: String -> [SingleEntry] -> Action
multiple comment ss = Post comment $ BalancedEntry ss

close :: Name -> Action
close = Close   

cr :: Name -> Amount -> SingleEntry
cr = Single Credit

dr :: Name -> Amount -> SingleEntry
dr = Single Debit

exampleStream :: [Action] 
exampleStream = [
    assets ["cash", "ar"],
    capital ["eq", "re"],
    liabilities ["ap", "dd", "tax"],
    incomes ["sales"],
    expenses ["salary"], 
    offset "eq" "ts",             
    offsets "sales" ["cashback", "refunds"],
    double "Shareholder investment" "cash" "eq" 1000,    
    multiple "Invoice with 5% sales tax" 
             [cr "sales" 500,
              cr "tax" 25,     
              dr "ar" 525],     
    double "Issued refund" "refunds" "ar" 25,    
    double "Issued cashback" "cashback" "cash" 75,     
    double "Paid salaries" "salary" "cash" 200, 
    close "re",                         
    double "Accrued dividend" "re" "dd" 100,         
    double "Paid dividend" "dd" "cash" 100,          
    double "Bought back shares" "ts" "cash" 50]      

module Closing where 

import Ledger 
import Chart (closingPairs, whichSide)
import Types
import qualified Data.Map as Map

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

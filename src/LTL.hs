module LTL
    ( LTL (..)
    , parseLTL
    ) where

import           Universum

data LTL
    = Negation LTL
    | Or LTL LTL
    | XOp LTL
    | UOp LTL LTL
    | Var String
    deriving (Show, Generic)

parseLTL :: String -> LTL
parseLTL = undefined

-- phi = p | not phi | phi or phi | phi and phi | phi -> phi | R phi | F phi | G phi
ltlParser = undefined

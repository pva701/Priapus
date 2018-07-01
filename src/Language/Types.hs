
-- | Parser types definitions

module Language.Types
       ( Parser
       , PError
       ) where

import Universum

import Text.Megaparsec

type PError = ParseError Char Void
type Parser = Parsec Void Text


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Universum

import           Buchi           (checkEmptiness, intersectBuchiAutomatons)
import           Conversion      (evalToBuchi)
import           Language.Driver (runProgramFile)
import           Language.Interpret (IState (..))
import           LTL             (LTL (Not), ltlToBuchiAutomaton, parseLTL, propositionals)

main :: IO ()
main = do
    [programFile, ltlFormulas] <- getArgs
    formulas <- lines <$> readFile ltlFormulas
    (_, IState{..}) <- runProgramFile programFile
    forM_ (zip [1::Int ..] formulas) $ \(i, fs) -> do
        let printf :: String -> IO ()
            printf s = putStrLn $ "Formula #" <> show i <> ": " <> s
        case parseLTL fs of
            Left e  -> printf $ show e
            Right f -> do
                putStrLn $ "ast: " ++ show f ++ "\n"
                let fba = ltlToBuchiAutomaton (Not f)
                let props = propositionals f
                case evalToBuchi props _iAutomaton of
                    Left e1   -> printf $ "Evalutation of formula failed, reason: " <> show e1
                    Right pba -> printf $ show $ checkEmptiness $ fba `intersectBuchiAutomatons` pba

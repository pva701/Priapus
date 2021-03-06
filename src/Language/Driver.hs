module Language.Driver where

import System.IO (hClose, hPutStrLn)
import Universum

import Language.Decl (parseProgram)
import Language.Expr (Value (..))
import Language.Interpret (IState, runProgram)

runProgramFull :: FilePath -> Text -> Either SomeException (Maybe Value, IState)
runProgramFull file txt =
    first toException (parseProgram file txt) >>=
    first toException . runProgram

runProgramFile :: FilePath -> IO (Maybe Value, IState)
runProgramFile file = do
    txt <- readFile file
    either throwM pure $ runProgramFull file txt

evalProgramFile :: FilePath -> IO (Maybe Value)
evalProgramFile = fmap fst . runProgramFile

evalProgramGolden :: FilePath -> FilePath -> IO ()
evalProgramGolden inp outp = do
    res <- evalProgramFile inp
    outh <- openFile outp WriteMode
    hPutStrLn outh $
        maybe "null" show res
    hClose outh

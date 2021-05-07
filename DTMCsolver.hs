{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State.Lazy ( evalStateT )
import Data.Attoparsec.ByteString.Char8 as AP ( parseOnly )
import qualified Data.ByteString as DB
import MCParser ( dtmcParse, dtmc_matrix )
import MCSolver ( dtmcSolver, dtmcSolverVerbose )
import qualified Numeric.LinearAlgebra as NL


---------------------Main-----------------------------------------------------------
main = do
    putStrLn "Enter the name of DTMC file"
    file <- getLine
    contents <- DB.readFile file
    putStrLn "How many iterations do you want to run?"
    n <- getLine
    putStrLn "Enable verbose output? (y/N)"
    v <- getChar
    let dtmc = parseOnly dtmcParse contents
    --Uncomment for compatibility mode:
    --let dtmc = parseOnly compDTMCParse contents

    if v == 'y' then do
        putStrLn "Probability matrix:"
        print (fmap dtmc_matrix dtmc)
        either
            (\err -> putStrLn "There was an issue with the input file: Not a valid DTMC.")
            (evalStateT (dtmcSolverVerbose (read n - 1)))
            dtmc
    else do
        either
            (\err -> putStrLn "There was an issue with the input file: Not a valid DTMC.")
            (evalStateT (dtmcSolver (read n - 1)))
            dtmc
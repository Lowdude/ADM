{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

-- Input format differs from that given in the course; indices start at 1 instead of 0. More details at end of this file

import Control.Monad.State.Lazy ( evalStateT )
import GHC.IO.Encoding ( setLocaleEncoding, utf8 )
import Data.Attoparsec.ByteString.Char8 as AP ( parseOnly )
import qualified Data.ByteString as DB
import MCParser ( dtmcParse )
import MCSolver ( dtmcSolver, dtmcSolverVerbose )

---------------------Main-----------------------------------------------------------
main = do
    setLocaleEncoding utf8
    putStrLn "Enter the name of DTMC file"
    file <- getLine
    contents <- DB.readFile file
    putStrLn "How many iterations do you want to run?"
    n <- getLine
    putStrLn "Enable verbose output? (y/N)"
    dtmc_vector <- getChar
    let dtmc = parseOnly dtmcParse contents
    --Uncomment for compatibility mode:
    --let dtmc = parseOnly compDTMCParse contents    

    if dtmc_vector == 'y' then do
        either
            (\err -> putStrLn "There was an issue with the input file: Not a valid DTMC.")
            (evalStateT (dtmcSolverVerbose (read n)))
            dtmc
    else do
        either
            (\err -> putStrLn "There was an issue with the input file: Not a valid DTMC.")
            (evalStateT (dtmcSolver (read n)))
            dtmc
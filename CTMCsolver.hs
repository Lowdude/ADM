{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State.Lazy ( evalStateT )
import GHC.IO.Encoding ( setLocaleEncoding, utf8 )
import Data.Attoparsec.ByteString.Char8 as AP ( parseOnly )
import qualified Data.ByteString as DB
import MCParser ( ctmcParse )
import MCSolver ( dtmcSolver, dtmcSolverVerbose, discretise )

---------------------Main-----------------------------------------------------------
main = do
    setLocaleEncoding utf8
    putStrLn "Enter the name of CTMC file"
    file <- getLine
    contents <- DB.readFile file
    putStrLn "How many iterations do you want to run?"
    n <- getLine
    putStrLn "What time step do you want to use for the discretisation?"
    l <- getLine
    putStrLn "Enable verbose output? (y/N)"
    ctmc_vector <- getChar
    let ctmc = parseOnly ctmcParse contents

    if ctmc_vector == 'y' then do
        either
            (\err -> putStrLn "There was an issue with the input file: Not a valid CTMC.")
            (evalStateT (dtmcSolverVerbose (read n)) <$> discretise (read l))
            ctmc
    else do
        either
            (\err -> putStrLn "There was an issue with the input file: Not a valid CTMC.")
            (evalStateT (dtmcSolver (read n)) <$> discretise (read l))
            ctmc

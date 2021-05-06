{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

-- Input format differs from that given in the course; indices start at 1 instead of 0. More details at end of this file

import Data.Matrix
import qualified Data.Vector as V
import Control.Monad.State.Lazy
import GHC.IO.Encoding ( setLocaleEncoding, utf8 )
import Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString as DB
import MCParser
import MCSolver

---------------------Calculating n steps of CTMC------------------------------------
iter :: State CTMC (Matrix Double)
iter = do
    ctmc <- get
    put $ CTMC (ctmc_matrix ctmc) (multStd2 (ctmc_vector ctmc) (ctmc_matrix ctmc))
    ctmc_vector <$> get

nIter :: Int -> StateT CTMC IO()
nIter n
    | n == 0 = do
        ctmc <- get
        liftIO $ print (ctmc_vector ctmc)
    | otherwise = do
        ctmc <- fmap (runState iter) get
        put (snd ctmc)
        nIter (n-1)


vIter :: Int -> StateT CTMC IO()
vIter n
    | n == 0 = do
        ctmc <- get
        liftIO $ print (ctmc_vector ctmc)
    | otherwise = do
        ctmc <- fmap (runState iter) get
        put (snd ctmc)
        liftIO $ print (ctmc_vector (snd ctmc))
        vIter (n-1)

---------------------Main-----------------------------------------------------------
main = do
    setLocaleEncoding utf8
    putStrLn "Enter the name of CTMC file"
    file <- getLine
    contents <- DB.readFile file
    putStrLn "How many iterations do you want to run?"
    n <- getLine
    putStrLn "Enable verbose output? (y/N)"
    ctmc_vector <- getChar
    let ctmc = parseOnly ctmcParse contents

    if ctmc_vector == 'y' then do
        either
            (\err -> putStrLn "There was an issue with the input file: Not a valid CTMC. Did you remember to end the input file with a newline character?")
            (evalStateT (vIter (read n)))
            ctmc
    else do
        either
            (\err -> putStrLn "There was an issue with the input file: Not a valid CTMC")
            (evalStateT (nIter (read n)))
            ctmc

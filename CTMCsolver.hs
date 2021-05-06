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

---------------------Calculating n steps of DTMC------------------------------------
iter :: State DTMC (Matrix Double)
iter = do
    dtmc <- get
    put $ DTMC (m dtmc) (multStd2 (v dtmc) (m dtmc))
    v <$> get

nIter :: Int -> StateT DTMC IO()
nIter n
    | n == 0 = do
        dtmc <- get
        liftIO $ print (v dtmc)
    | otherwise = do
        dtmc <- fmap (runState iter) get
        put (snd dtmc)
        nIter (n-1)


vIter :: Int -> StateT DTMC IO()
vIter n
    | n == 0 = do
        dtmc <- get
        liftIO $ print (v dtmc)
    | otherwise = do
        dtmc <- fmap (runState iter) get
        put (snd dtmc)
        liftIO $ print (v (snd dtmc))
        vIter (n-1)

---------------------Main-----------------------------------------------------------
main = do
    setLocaleEncoding utf8
    putStrLn "Enter the name of DTMC file"
    file <- getLine
    contents <- DB.readFile file
    putStrLn "How many iterations do you want to run?"
    n <- getLine
    putStrLn "Enable verbose output? (y/N)"
    v <- getChar
    let dtmc = parseOnly dtmcParse contents

    if v == 'y' then do
        either
            (\err -> putStrLn "There was an issue with the input file: Not a valid DTMC. Did you remember to end the input file with a newline character?")
            (evalStateT (vIter (read n)))
            dtmc
    else do
        either
            (\err -> putStrLn "There was an issue with the input file: Not a valid DTMC")
            (evalStateT (nIter (read n)))
            dtmc
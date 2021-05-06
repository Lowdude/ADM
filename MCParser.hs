module MCParser (DTMC(..),dtmcParse,compDTMCParse) where

import Data.Matrix ( getRow, setElem, zero, Matrix(nrows) )
import qualified Data.Vector as V
import Data.Attoparsec.ByteString.Char8 as AP
    ( decimal, double, space, endOfLine, sepBy, Parser, skipMany )
import qualified Data.ByteString as DB

---------------------Data constructors----------------------------------------------
data DTMC = DTMC {
    dtmc_matrix :: Matrix Double
    , dtmc_vector :: Matrix Double
}

data CTMC = CTMC {
    ctmc_matrix :: Matrix Double
    , ctmc_vector :: Matrix Double
}

data Transition = Transition {
    start :: Int
    , end :: Int
    , prob :: Double
}

instance Show DTMC where
    show (DTMC dtmc_matrix dtmc_vector) = "Probability Matrix:\n" ++ show dtmc_matrix ++ "\n State: \n" ++ show dtmc_vector ++ "\n"

instance Show CTMC where
    show (CTMC ctmc_matrix ctmc_vector) = "Probability Matrix:\n" ++ show ctmc_matrix ++ "\n State: \n" ++ show ctmc_vector ++ "\n"

---------------------Parsing--------------------------------------------------------
dtmcParse :: Parser DTMC
dtmcParse = do
    states <- decimal
    endOfLine
    initState <- decimal
    endOfLine
    listOfTransitions <- sepBy _transParse endOfLine
    return $ _buildDTMC states initState listOfTransitions

ctmcParse :: Parser CTMC
ctmcParse = do
    states <- decimal
    endOfLine
    initState <- decimal
    endOfLine
    listOfTransitions <- sepBy _transParse endOfLine
    return $ _buildCTMC states initState listOfTransitions

compDTMCParse :: Parser DTMC
compDTMCParse = do
    states <- decimal
    endOfLine
    decimal -- number of transitions not required in advance, so it will be disregarded
    endOfLine
    listOfTransitions <- sepBy _transParse endOfLine
    return $ _buildDTMC states 1 (fmap _compatTrans listOfTransitions)

_transParse :: Parser Transition
_transParse = do
    start <- decimal
    skipMany space
    end <- decimal
    skipMany space
    Transition start end <$> double

---------------------Building DTMC from parsed contents-----------------------------
_buildDTMC :: Int -> Int -> [Transition] -> DTMC
_buildDTMC n s t = DTMC (_normaliseDTMC 1 $ _buildMatrix (zero n n) t) (_buildVector n s)

_buildCTMC :: Int -> Int -> [Transition] -> CTMC
_buildCTMC n s t = CTMC (_normaliseCTMC 1 $ _buildMatrix (zero n n) t) (_buildVector n s)


_buildVector :: Int -> Int -> Matrix Double
_buildVector numberOfStates startingNode = setElem 1 (1,startingNode) (zero 1 numberOfStates)

_buildMatrix :: Matrix Double -> [Transition] -> Matrix Double
_buildMatrix dtmc_matrix t
    | null t = dtmc_matrix
    | otherwise = setElem (prob t1) (start t1,end t1) (_buildMatrix dtmc_matrix (tail t)) where t1 = head t

_normaliseDTMC :: Num a => Int -> Matrix a -> Matrix a
_normaliseDTMC n dtmc_matrix
    | n > nrows dtmc_matrix = dtmc_matrix
    | otherwise = _normaliseDTMC (n+1) $ setElem (1 - V.sum (getRow n dtmc_matrix)) (n,n) dtmc_matrix

_normaliseCTMC :: Num a => Int -> Matrix a -> Matrix a
_normaliseCTMC n ctmc_matrix
    | n > nrows ctmc_matrix = ctmc_matrix
    | otherwise = _normaliseDTMC (n+1) $ setElem (- V.sum (getRow n ctmc_matrix)) (n,n) ctmc_matrix

---------------------Compatibility functions----------------------------------------

_compatTrans :: Transition -> Transition -- shifting indices to start from 1 instead of 0
_compatTrans t = Transition (start t + 1) (end t + 1) (prob t)
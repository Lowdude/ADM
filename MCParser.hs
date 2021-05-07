module MCParser (DTMC(..),CTMC(..),dtmcParse,compDTMCParse,ctmcParse,compCTMCParse) where

import Numeric.LinearAlgebra as NL
    ( rows,
      toRows,
      assoc,
      ident,
      sumElements,
      (?),
      diagl,
      Matrix,
      Vector )
import qualified Data.Vector as V
import Data.Attoparsec.ByteString.Char8 as AP
    ( decimal, double, space, endOfLine, sepBy, skipMany1, Parser )
import qualified Data.ByteString as DB

---------------------Data constructors----------------------------------------------
data DTMC = DTMC {
    dtmc_matrix :: NL.Matrix Double
    , dtmc_vector :: NL.Vector Double
}

data CTMC = CTMC {
    ctmc_matrix :: NL.Matrix Double
    , ctmc_vector :: NL.Vector Double
}

instance Show DTMC where
    show (DTMC dtmc_matrix dtmc_vector) = "Probability NL.Matrix:\n" ++ show dtmc_matrix ++ "\n State: \n" ++ show dtmc_vector ++ "\n"

instance Show CTMC where
    show (CTMC ctmc_matrix ctmc_vector) = "Probability NL.Matrix:\n" ++ show ctmc_matrix ++ "\n State: \n" ++ show ctmc_vector ++ "\n"

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
    return $ _buildDTMC states 1 listOfTransitions

compCTMCParse :: Parser CTMC
compCTMCParse = do
    states <- decimal
    endOfLine
    decimal -- number of transitions not required in advance, so it will be disregarded
    endOfLine
    listOfTransitions <- sepBy _transParse endOfLine
    return $ _buildCTMC states 1 listOfTransitions

_transParse :: Parser ((Int,Int),Double)
_transParse = do
    start <- decimal
    skipMany1 space
    end <- decimal
    skipMany1 space
    prob <- AP.double
    return ((start, end),prob)

---------------------Build helpers from parsed contents-----------------------------
_buildDTMC :: Int -> Int -> [((Int,Int),Double)] -> DTMC
_buildDTMC n s t = DTMC (_normaliseDTMC 1 $ NL.assoc (n,n) 0 t) (_buildVector n s)

_buildCTMC :: Int -> Int -> [((Int,Int),Double)] -> CTMC
_buildCTMC n s t = CTMC (_normaliseCTMC 1 $ NL.assoc (n,n) 0 t) (_buildVector n s)

_buildVector :: Int -> Int -> NL.Vector Double
_buildVector numberOfStates startingNode = head $ NL.toRows $ NL.assoc (1,numberOfStates) 0 [((0,startingNode),1)]

_normaliseDTMC :: Int -> NL.Matrix Double -> NL.Matrix Double
_normaliseDTMC n m = m + NL.ident (NL.rows m) - NL.diagl (_rowsum m 0)

_normaliseCTMC :: Int -> NL.Matrix Double -> NL.Matrix Double
_normaliseCTMC n m = m - NL.diagl (_rowsum m 0)

_rowsum :: NL.Matrix Double -> Int -> [Double]
_rowsum m n
    | n >= NL.rows m = []
    | otherwise = NL.sumElements (m ? [n]): _rowsum m (n+1)
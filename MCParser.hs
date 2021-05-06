module MCParser (DTMC(..),dtmcParse,compDTMCParse) where

import Data.Matrix ( getRow, setElem, zero, Matrix(nrows) )
import qualified Data.Vector as V
import Data.Attoparsec.ByteString.Char8 as AP
    ( decimal, double, space, endOfLine, sepBy, Parser )
import qualified Data.ByteString as DB

---------------------Data constructors----------------------------------------------
data DTMC = DTMC {
    m :: Matrix Double
    , v :: Matrix Double
}

data Transition = Transition {
    start :: Int
    , end :: Int
    , prob :: Double
}

instance Show DTMC where
    show (DTMC m v) = "Probability Matrix:\n" ++ show m ++ "\n State: \n" ++ show v ++ "\n"

---------------------Parsing--------------------------------------------------------
dtmcParse :: Parser DTMC
dtmcParse = do
    states <- decimal
    endOfLine
    initState <- decimal
    endOfLine
    listOfTransitions <- sepBy _transParse endOfLine
    return $ _buildDTMC states initState listOfTransitions

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
    space
    end <- decimal
    space
    Transition start end <$> double

---------------------Building DTMC from parsed contents-----------------------------
_buildDTMC :: Int -> Int -> [Transition] -> DTMC
_buildDTMC n s trans = DTMC (_buildMatrix n trans) (_buildVector n s)

_buildVector :: Int -> Int -> Matrix Double
_buildVector numberOfStates startingNode = setElem 1 (1,startingNode) (zero 1 numberOfStates)

_buildMatrix :: Int -> [Transition] -> Matrix Double
_buildMatrix n t = _normalise 1 $ _buildHelper (zero n n) t

_buildHelper :: Matrix Double -> [Transition] -> Matrix Double
_buildHelper m t
    | null t = m
    | otherwise = setElem (prob t1) (start t1,end t1) (_buildHelper m (tail t)) where t1 = head t

_normalise :: Num a => Int -> Matrix a -> Matrix a
_normalise n m
    | n > nrows m = m
    | otherwise = _normalise (n+1) $ setElem (1 - V.sum (getRow n m)) (n,n) m

---------------------Compatibility functions----------------------------------------

_compatTrans :: Transition -> Transition -- shifting indices to start from 1 instead of 0
_compatTrans t = Transition (start t + 1) (end t + 1) (prob t)
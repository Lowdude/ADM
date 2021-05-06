module MCParser (DTMC(..),Transition,dtmcParse) where

import Data.Matrix ( getRow, setElem, zero, Matrix(nrows) )
import qualified Data.Vector as V
import Data.Attoparsec.ByteString.Char8 as AP
    ( decimal, double, space, endOfLine, manyTill, endOfInput, Parser )
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
    listOfTransitions <- manyTill transParse endOfInput
    return $ buildDTMC states initState listOfTransitions

transParse :: Parser Transition
transParse = do
    start <- decimal
    space
    end <- decimal
    space
    prob <- double
    endOfLine
    return $ Transition start end prob

---------------------Building DTMC from parsed contents-----------------------------
buildDTMC :: Int -> Int -> [Transition] -> DTMC
buildDTMC n s trans = DTMC (buildMatrix n trans) (buildVector n s)

buildVector :: Int -> Int -> Matrix Double
buildVector numberOfStates startingNode = setElem 1 (1,startingNode) (zero 1 numberOfStates)

buildMatrix :: Int -> [Transition] -> Matrix Double
buildMatrix n t = normalise 1 $ _buildHelper (zero n n) t

_buildHelper :: Matrix Double -> [Transition] -> Matrix Double
_buildHelper m t
    | null t = m
    | otherwise = setElem (prob t1) (start t1,end t1) (_buildHelper m (tail t)) where t1 = head t

normalise :: Num a => Int -> Matrix a -> Matrix a
normalise n m
    | n > nrows m = m
    | otherwise = normalise (n+1) $ setElem (1 - V.sum (getRow n m)) (n,n) m
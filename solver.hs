{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Matrix
import qualified Data.Vector as V
import Control.Monad.State.Lazy
import GHC.IO.Encoding
import Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString as DB

---------------------Data constructors----------------------------------------------
data DTMC = DTMC (Matrix Double) (Matrix Double)
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

---------------------Teststructures-------------------------------------------------
testVector = fromLists [[0,0,1,0,0]]
testMatrix = fromLists [[1,0,0,0,0],[0.5,0,0.5,0,0],[0,0.5,0,0.5,0],[0,0,0.5,0,0.5],[0,0,0,0,1]]
testDTMC = DTMC testMatrix testVector

testCall :: Int -> IO ()
testCall n
    | n == 0 = putStrLn "that was it"
    | otherwise = do
        putStrLn "yoyoyo"
        testCall (n-1)

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

---------------------Calculating n steps of DTMC------------------------------------
iter :: DTMC -> DTMC
iter (DTMC m v) = DTMC m (multStd2 v m)

nIter :: Int -> DTMC -> DTMC
nIter n d
    | n == 0 = d
    | otherwise = nIter (n-1) (iter d)

--nIterV :: Int -> Int -> DTMC -> IO DTMC
nIterV c n (DTMC m v) = do
    putStrLn ("Step " ++ show c ++ ": " ++ show v)
    if n==0 then do return (DTMC m v)
    else do nIterV (c+1) (n-1) (iter (DTMC m v))

--instance Show (IO DTMC) where
--    show _ = " asd "

---------------------Main-----------------------------------------------------------
main = do
    setLocaleEncoding utf8
    -- putStrLn "Enter the name of DTMC file"
    -- file <- getLine 
    -- contents <- DB.readFile file
    -- putStrLn "How many iterations do you want to run?"
    -- n <- getLine
    --putStrLn "Enable verbose output? (y/N)"
    --v <- getChar
    contents <- DB.readFile "example.txt"
    let parsedStuff = parseOnly dtmcParse contents
    print $ fmap (nIterV 0 10) parsedStuff
    
    --return $ fmap (nIterV 0 (read n)) (parseOnly dtmcParse contents)
    -- if v == 'y' then do
    --     return $ fmap (nIterV (read n)) (parseOnly dtmcParse contents)
    -- else do
    --     return $ fmap (nIterV (read n)) (parseOnly dtmcParse contents)
    --print $ fmap (nIter (read n)) (parseOnly dtmcParse contents)


---------------------Comments regarding the input format----------------------------
-- required file format:
-- numberOfStates
-- initialState
-- s1 e1 p1 # where s1 is the outgoing node, e1 is the ingoing node and p1 is the associated probability 
-- s2 e2 p2
-- ... 
-- # Nodes are numbered from 1 to n
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Matrix
    ( fromLists, getRow, multStd2, setElem, zero, Matrix(nrows) )
import qualified Data.Vector as V
import Control.Monad.State.Lazy
    ( evalStateT,
      runState,
      MonadIO(liftIO),
      MonadState(put, get),
      State,
      StateT )
import GHC.IO.Encoding ( setLocaleEncoding, utf8 )
import Data.Attoparsec.ByteString.Char8 as AP
    ( decimal,
      double,
      space,
      endOfLine,
      parseOnly,
      manyTill,
      endOfInput,
      Parser )
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

---------------------Teststructures-------------------------------------------------
testVector = fromLists [[0,0,1,0,0]]
testMatrix = fromLists [[1,0,0,0,0],[0.5,0,0.5,0,0],[0,0.5,0,0.5,0],[0,0,0.5,0,0.5],[0,0,0,0,1]]
testDTMC = DTMC testMatrix testVector

testCall :: Int -> IO ()
testCall n
    | n == 0 = putStrLn "this is a test string"
    | otherwise = do
        putStrLn "this is another test string"
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
            (\err -> putStrLn "There was an issue with the input file: Not a valid DTMC")
            (evalStateT (vIter (read n)))
            dtmc
    else do
        either
            (\err -> putStrLn "There was an issue with the input file: Not a valid DTMC")
            (evalStateT (nIter (read n)))
            dtmc

---------------------Comments regarding the input format----------------------------
-- required file format:
-- numberOfStates
-- initialState
-- s1 e1 p1 # where s1 is the outgoing node, e1 is the ingoing node and p1 is the associated probability 
-- s2 e2 p2
-- ... 
-- # Nodes are numbered from 1 to n
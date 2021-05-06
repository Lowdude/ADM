module MCSolver (dtmcSolver, dtmcSolverVerbose, discretise) where
import MCParser ( DTMC(..), CTMC(..) )
import Data.Matrix
    ( identity, multStd2, scaleMatrix, Matrix(nrows) )
import qualified Data.Vector as V
import Control.Monad.State.Lazy
    ( MonadIO(liftIO), StateT, MonadState(put, get), runState, State )

_iter :: State DTMC (Matrix Double)
_iter = do
    dtmc <- get
    put $ DTMC (dtmc_matrix dtmc) (multStd2 (dtmc_vector dtmc) (dtmc_matrix dtmc))
    dtmc_vector <$> get

dtmcSolver :: Int -> StateT DTMC IO()
dtmcSolver n
    | n == 0 = do
        dtmc <- get
        liftIO $ print (dtmc_vector dtmc)
    | otherwise = do
        dtmc <- fmap (runState _iter) get
        put (snd dtmc)
        dtmcSolver (n-1)

dtmcSolverVerbose :: Int -> StateT DTMC IO()
dtmcSolverVerbose n
    | n == 0 = do
        dtmc <- get
        liftIO $ print (dtmc_vector dtmc)
    | otherwise = do
        dtmc <- fmap (runState _iter) get
        put (snd dtmc)
        liftIO $ print (dtmc_vector (snd dtmc))
        dtmcSolverVerbose (n-1)

discretise :: Double -> CTMC -> DTMC
discretise x ctmc = DTMC (identity (nrows m) + scaleMatrix x m)(ctmc_vector ctmc) where m = ctmc_matrix ctmc
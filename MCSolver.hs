module MCSolver (dtmcSolver, dtmcSolverVerbose, discretise) where
import MCParser ( DTMC(..), CTMC(..) )
import Numeric.LinearAlgebra as NL
    ( (<#), asRow, rows, ident, disp, Linear(scale), Vector )
import qualified Data.Vector as V
import Control.Monad.State.Lazy
    ( MonadIO(liftIO), StateT, MonadState(put, get), runState, State )

_iter :: State DTMC (Vector Double)
_iter = do
    dtmc <- get
    put $ DTMC (dtmc_matrix dtmc) (dtmc_vector dtmc <# dtmc_matrix dtmc)
    dtmc_vector <$> get

dtmcSolver :: Int -> StateT DTMC IO()
dtmcSolver n
    | n == 0 = do
        dtmc <- get
        liftIO $ disp 4 $ asRow (dtmc_vector dtmc)
    | otherwise = do
        dtmc <- fmap (runState _iter) get
        put (snd dtmc)
        dtmcSolver (n-1)

dtmcSolverVerbose :: Int -> StateT DTMC IO()
dtmcSolverVerbose n
    | n == 0 = do
        dtmc <- get
        liftIO $ putStrLn "Finished"
    | otherwise = do
        dtmc <- fmap (runState _iter) get
        put (snd dtmc)
        liftIO $ disp 4 $ asRow (dtmc_vector (snd dtmc)) -- Specify printed precision here
        dtmcSolverVerbose (n-1)

discretise :: Double -> CTMC -> DTMC
discretise x ctmc = DTMC (ident (rows m) + scale x m)(ctmc_vector ctmc) where m = ctmc_matrix ctmc
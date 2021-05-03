import Control.Monad.Random as CM
    ( uniform, weighted, evalRandIO, Rand, RandomGen )

data Coin = Tails | Heads deriving (Show,Eq)

fairToss :: (RandomGen g) => Rand g Coin
fairToss = uniform [Tails,Heads]

weightedToss :: (RandomGen g) => Rational -> Rational -> Rand g Coin
weightedToss h t = weighted [(Heads,h),(Tails,t)]

fGamble :: (RandomGen g) => Rand g (Int,Int)
fGamble = _gamble (100,100) 1 1

uGamble :: (RandomGen g) => Rational -> Rational -> Rand g (Int,Int)
uGamble = _gamble (100,100)

_gamble :: (RandomGen g) => (Int,Int) -> Rational -> Rational -> Rand g (Int,Int)
_gamble (a,b) p1 p2
   | a == 0 = return (a,b)
   | b == 0 = return (a,b)
   | otherwise = do
      coin <- weightedToss p1 p2
      case coin of
         Heads -> _gamble (a+50,b-50) p1 p2
         Tails -> _gamble (a-50,b+50) p1 p2

multFairGames :: (RandomGen g) => (Int,Int) -> Int -> Rand g (Int, Int)
multFairGames (c1,c2) n = multUnfairGames (c1,c2) n 1 1

multUnfairGames :: (RandomGen g) => (Int,Int) -> Int -> Rational -> Rational -> Rand g (Int, Int)
multUnfairGames (c1,c2) n p1 p2
   | n == 0 = return (c1,c2)
   | otherwise = do
      res <- uGamble p1 p2
      case fst res of
         0 -> multUnfairGames (c1,c2 + 1) (n-1) p1 p2
         200 -> multUnfairGames (c1 + 1,c2) (n-1) p1 p2

main = do
   mGames <- evalRandIO (multFairGames (0,0) 1000)
   print mGames -- Notice: Game appears to be fair
   mUnfairGames <- evalRandIO (multUnfairGames (0,0) 100000 (1/10) (9/10))
   print mUnfairGames
   -- apparently no start-bias, sample size used: 100000
   -- 2/3 vs 1/3 ends up as 4/5 vs 1/5 win chance, same for 1/3 vs 2/3
   -- 3/4 -> 9/10
   -- 0.9 -> ca. 0.988
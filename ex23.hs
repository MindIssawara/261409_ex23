-- can you reuse code from the previous version?
--  Yes, I reused from Aj. Chin's version and I modified some.



import System.Random

-- try to separate your code into pure and impure parts
-- pure parts
type GuessLog = [Int]

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader r r
ask = Reader id

instance Functor (Reader r) where
  fmap f (Reader rf) = Reader $ f . rf

instance Applicative (Reader r) where
  pure x = Reader $ const x
  (Reader rf) <*> (Reader rx) = Reader $ \r -> rf r (rx r)

instance Monad (Reader r) where
  (Reader ra) >>= f = Reader $ \r -> runReader (f (ra r)) r

newtype Writer w a = Writer { runWriter :: (a, w) }

tell :: w -> Writer w ()
tell log = Writer ((), log)

instance Functor (Writer w) where
  fmap f (Writer (x, log)) = Writer (f x, log)

instance Monoid w => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  (Writer (f, log1)) <*> (Writer (x, log2)) = Writer (f x, log1 <> log2)

instance Monoid w => Monad (Writer w) where
  (Writer (x, log1)) >>= f =
    let (Writer (y, log2)) = f x
    in Writer (y, log1 <> log2)

randNum :: RandomGen g => g -> Int
randNum gen = fst $ uniform gen

readNumber :: String -> IO Int
readNumber msg = do
    putStr $ msg ++ ": "
    line <- getLine
    case reads line of
      [(n, "")] -> return n
      _ -> do
         putStrLn "Invalid input. Please enter a number."
         readNumber msg

verdict' :: Ord a => a -> a -> (Maybe a, Maybe a) -> Either (String, (Maybe a, Maybe a)) String
verdict' target guess (lo, hi) =
  case compare guess target of
     EQ -> Right "You win!"
     LT -> Left ("Too low", (Just guess, hi))
     GT -> Left ("Too high", (lo, Just guess))

inRange :: Ord a => (Maybe a, Maybe a) -> a -> Bool
inRange (lo, hi) guess =
  maybe True (< guess) lo && maybe True (> guess) hi

getRange :: IO (Int, Int)
getRange = do
  lo <- readNumber "Lower bound"
  hi <- readNumber "Upper bound"
  if lo > hi then do
    putStrLn "Invalid range"
    getRange
  else return (lo, hi)

-- impure parts
readGuess :: (Maybe Int, Maybe Int) -> IO Int
readGuess range = do
  guess <- readNumber "Guess"
  if inRange range guess
    then return guess
    else do
      putStrLn "Impossible answer"
      readGuess range

combineWriter :: Writer GuessLog () -> Writer GuessLog () -> Writer GuessLog ()
combineWriter (Writer ((), log1)) (Writer ((), log2)) = Writer ((), log1 <> log2)

runGameRg :: Int -> (Maybe Int, Maybe Int) -> Int -> Reader Int (IO (Writer GuessLog ()))
runGameRg num range count = Reader $ \lim -> do
    guess <- readGuess range
    let v = verdict' num guess range
    case v of
      Right msg -> do
         putStrLn msg
         return (Writer ((), [guess]))
      Left (msg, newRange) -> do
         putStrLn msg
         if count < lim then do
            next <- runReader (runGameRg num newRange (count + 1)) lim
            let current = Writer ((), [guess])
            return (combineWriter current next)
         else do
            putStrLn "Game over"
            return (Writer ((), [guess]))

v5 :: IO ()
v5 = do
    g <- newStdGen
    range <- getRange
    lim <- readNumber "Guess limit"
    let (num, _) = uniformR range g
    writerResult <- runReader (runGameRg num (Nothing, Nothing) 1) lim
    let (_, log) = runWriter writerResult
    putStrLn $ "Numbers guessed: " ++ unwords (map show log)

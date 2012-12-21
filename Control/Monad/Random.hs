{-# LANGUAGE TypeFamilies #-}

{- |
Copyright    : 2006-2012 Cale Gibbard, Russell O'Connor, Dan Doel, Remi Turk, Eric Kidd, Michael Ivko.
License      : OtherLicense
Stability    : experimental
Portability  : non-portable (type families)

A random number generation monad.  See
<http://www.haskell.org/haskellwiki/NewMonads/MonadRandom> for the original
version of this code.

The actual interface is defined by
'Control.Monad.Random.Class.MonadRandom'.

[Computation type:] Computations which consume random values.

[Binding strategy:] The computation proceeds in the same fashion as the
identity monad, but it carries a random number generator that may be
queried to generate random values.

[Useful for:] Monte Carlo algorithms and simulating random processes.

-}

module Control.Monad.Random (
    module System.Random,
    module Control.Monad.Random.Class,
    evalRandT,
    runRandT,
    evalRand,
    runRand,
    evalRandIO,
    fromList,
    Rand, RandT -- but not the data constructors
    -- * Example
    -- $RandExample
    ) where

import System.Random
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Random.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.Writer
import Control.Arrow
import Control.Applicative

-- | A monad transformer which adds a random number generator to an
-- existing monad.
newtype RandT g m a = RandT{
    unRandT :: StateT g m a
}

-- Handwirtten instances, because apparently type fammilies don't go well with
-- GeneralizedNewtypeDeriving.
instance Functor m => Functor (RandT g m) where
    fmap f = RandT . fmap f . unRandT
instance Monad m => Monad (RandT g m) where
    (>>=) ma f = RandT $ unRandT ma >>= unRandT . f
    (>>) ma mb = RandT $ unRandT ma >> unRandT mb
    return = RandT . return
    fail = RandT . fail
instance MonadTrans (RandT g) where
    lift = RandT . lift
instance MonadIO m => MonadIO (RandT g m) where
    liftIO = RandT . liftIO
instance MonadFix m => MonadFix (RandT g m) where
    mfix = RandT . mfix . (unRandT .)

instance (Functor m,Monad m) => Applicative (RandT g m) where
  pure = return
  (<*>) = ap

liftState :: (MonadState m) => (StateType m -> (a, StateType m)) -> m a
liftState t = do v <- get
                 let (x, v') = t v
                 put v'
                 return x

instance (Monad m, RandomGen g) => MonadRandom (RandT g m) where
    getRandom = RandT . liftState $ random
    getRandoms = RandT . liftState $ first randoms . split
    getRandomR (x,y) = RandT . liftState $ randomR (x,y)
    getRandomRs (x,y) = RandT . liftState $
                            first (randomRs (x,y)) . split

instance (Monad m, RandomGen g) => MonadSplit (RandT g m) where
    type SplitType (RandT g m) = g
    getSplit = RandT . liftState $ split

-- | Evaluate a RandT computation using the generator @g@.  Note that the
-- generator @g@ is not returned, so there's no way to recover the
-- updated version of @g@.
evalRandT :: (Monad m, RandomGen g) => RandT g m a -> g -> m a
evalRandT (RandT x) g = evalStateT x g

-- | Run a RandT computation using the generator @g@, returning the result and
-- the updated generator.
runRandT  :: (Monad m, RandomGen g) => RandT g m a -> g -> m (a, g)
runRandT (RandT x) g = runStateT x g

-- | A basic random monad.
newtype Rand g a = Rand{
    unRand :: (RandT g Identity a)
}

-- More handwritten instances.
instance Functor (Rand g) where
    fmap f = Rand . fmap f . unRand
instance Applicative (Rand g) where
    pure = Rand . pure
    (<*>) fab fa = Rand $ unRand fab <*> unRand fa
instance Monad (Rand g) where
    (>>=) ma f = Rand $ unRand ma >>= unRand . f
    (>>) ma mb = Rand $ unRand ma >> unRand mb
    return = Rand . return
    fail = Rand . fail
instance (RandomGen g) => MonadRandom (Rand g) where
    getRandom = Rand getRandom
    getRandoms = Rand getRandoms
    getRandomR = Rand . getRandomR
    getRandomRs = Rand . getRandomRs
instance (RandomGen g) => MonadSplit (Rand g) where
    type SplitType (Rand g) = g
    getSplit = Rand getSplit
instance MonadFix (Rand g) where
    mfix = Rand . mfix . (unRand .)

-- | Evaluate a random computation using the generator @g@.  Note that the
-- generator @g@ is not returned, so there's no way to recover the
-- updated version of @g@.
evalRand :: (RandomGen g) => Rand g a -> g -> a
evalRand (Rand x) g = runIdentity (evalRandT x g)

-- | Run a random computation using the generator @g@, returning the result
-- and the updated generator.
runRand :: (RandomGen g) => Rand g a -> g -> (a, g)
runRand (Rand x) g = runIdentity (runRandT x g)

-- | Evaluate a random computation in the IO monad, splitting the global standard generator to get a new one for the computation.
evalRandIO :: Rand StdGen a -> IO a
evalRandIO x = fmap (evalRand x) newStdGen

-- | Sample a random value from a weighted list.  The total weight of all
-- elements must not be 0.
fromList :: (MonadRandom m) => [(a,Rational)] -> m a
fromList [] = error "MonadRandom.fromList called with empty list"
fromList [(x,_)] = return x
fromList xs = do
  -- TODO: Do we want to be able to use floats as weights?
  -- TODO: Better error message if weights sum to 0.
  let s = (fromRational (sum (map snd xs))) :: Double -- total weight
      cs = scanl1 (\(_,q) (y,s') -> (y, s'+q)) xs       -- cumulative weight
  p <- liftM toRational $ getRandomR (0.0,s)
  return . fst . head $ dropWhile (\(_,q) -> q < p) cs

instance (MonadRandom m) => MonadRandom (StateT s m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadRandom m, Monoid w) => MonadRandom (WriterT w m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadRandom m) => MonadRandom (ReaderT r m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (Error e, MonadRandom m) => MonadRandom (ErrorT e m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadSplit m) => MonadSplit (StateT s m) where
    type SplitType (StateT s m) = SplitType m
    getSplit = lift getSplit

instance (MonadSplit m, Monoid w) => MonadSplit (WriterT w m) where
    type SplitType (WriterT w m) = SplitType m
    getSplit = lift getSplit

instance (MonadSplit m) => MonadSplit (ReaderT r m) where
    type SplitType (ReaderT r m) = SplitType m
    getSplit = lift getSplit

instance (Error e, MonadSplit m) => MonadSplit (ErrorT e m) where
    type SplitType (ErrorT e m) = SplitType m
    getSplit = lift getSplit

instance (MonadState m, RandomGen g) => MonadState (RandT g m) where
    type StateType (RandT g m) = StateType m
    get = lift get
    put = lift . put

instance (MonadReader m, RandomGen g) => MonadReader (RandT g m) where
    type EnvType (RandT g m) = EnvType m
    ask = lift ask
    local f (RandT m) = RandT $ local f m

instance (MonadWriter m, RandomGen g) => MonadWriter (RandT g m) where
    type WriterType (RandT g m) = WriterType m
    tell = lift . tell
    listen (RandT m) = RandT $ listen m
    pass (RandT m) = RandT $ pass m

instance MonadRandom IO where
    getRandom = randomIO
    getRandomR = randomRIO
    getRandoms = fmap randoms newStdGen
    getRandomRs b = fmap (randomRs b) newStdGen

instance MonadSplit IO where
    type SplitType IO = StdGen
    getSplit = newStdGen

{- $RandExample

The @die@ function simulates the roll of a die, picking a number between 1
and 6, inclusive, and returning it in the 'Rand' monad.  Notice that this
code will work with any source of random numbers @g@.

>die :: (RandomGen g) => Rand g Int
>die = getRandomR (1,6)

The @dice@ function uses @replicate@ and @sequence@ to simulate the roll of
@n@ dice.

>dice :: (RandomGen g) => Int -> Rand g [Int]
>dice n = sequence (replicate n die)

To extract a value from the 'Rand' monad, we can can use 'evalRandIO'.

>main = do
>  values <- evalRandIO (dice 2)
>  putStrLn (show values)

-}

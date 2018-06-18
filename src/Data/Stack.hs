module Data.Stack (StackT
                 , Stack
                 , nullStackT
                 , runStackT
                 , evalStackT
                 , execStackT
                 , nullStack
                 , runStack
                 , evalStack
                 , execStack
                 , push
                 , pop) where

import Control.Arrow (first)
import Data.Function.Combinators ((...))
import Control.Monad.Identity (Identity, runIdentity)
import Data.Maybe (listToMaybe)

-- StackT

newtype StackT a m r = StackT { runStackT :: [a] -> m (r, [a]) }

instance Functor m => Functor (StackT a m) where
  fmap f (StackT computation) =
    StackT $ fmap (first f) . computation

instance Monad m => Applicative (StackT a m) where
  pure result = StackT $ \stack -> pure (result, stack)

  (StackT computationA) <*> (StackT computationB) =
    StackT $ \stack -> do
      (resultA, newStackA) <- computationA stack
      (resultB, newStackB) <- computationB newStackA

      return (resultA resultB, newStackB)

instance Monad m => Monad (StackT a m) where
  (StackT computation) >>= f =
    StackT $ \stack -> do
      (firstResult, newStackA) <- computation stack
      runStackT (f firstResult) newStackA

nullStackT :: Monad m => StackT a m ()
nullStackT = pure ()

evalStackT :: Functor m => StackT a m r -> [a] -> m r
evalStackT = fmap fst ... runStackT

execStackT :: Functor m => StackT a m r -> [a] -> m [a]
execStackT = fmap snd ... runStackT

-- Stack

type Stack a r = StackT a Identity r

nullStack :: Stack a ()
nullStack = pure ()

runStack :: Stack a r -> [a] -> (r, [a])
runStack = runIdentity ... runStackT

evalStack :: Stack a r -> [a] -> r
evalStack = fst ... runStack

execStack :: Stack a r -> [a] -> [a]
execStack = snd ... runStack

-- StackT operations

pop :: Monad m => StackT a m (Maybe a)
pop = StackT $ \stack ->
  return (safeHead stack, drop 1 stack)
    where
      safeHead = listToMaybe

push :: Monad m => a -> StackT a m ()
push x = StackT $ \stack ->
  return ((), x : stack)

module Data.Stack (Stack
                 , nullStack
                 , runStack
                 , evalStack
                 , execStack
                 , push
                 , pop) where

import Control.Arrow (first)
import Data.Function.Combinators ((...))
import Data.Maybe (listToMaybe)

newtype Stack a r = Stack { runStack :: [a] -> (r, [a]) }

instance Functor (Stack a) where
  fmap f (Stack computation) =
    Stack $ first f . computation

instance Applicative (Stack a) where
  pure result = Stack $ \stack -> (result, stack)

  (Stack computationA) <*> (Stack computationB) =
    Stack $ \stack ->
      let
        (resultA, newStackA) = computationA stack
        (resultB, newStackB) = computationB newStackA
      in
        (resultA resultB, newStackB)

instance Monad (Stack a) where
  (Stack computation) >>= f =
    Stack $ \stack ->
      let
        (firstResult, newStackA) = computation stack
      in
        runStack (f firstResult) newStackA

nullStack :: Stack a ()
nullStack = pure ()

evalStack :: Stack a r -> [a] -> r
evalStack = fst ... runStack

execStack :: Stack a r -> [a] -> [a]
execStack = snd ... runStack

pop :: Stack a (Maybe a)
pop = Stack $ \stack ->
  (safeHead stack, drop 1 stack)
    where
      safeHead = listToMaybe

push :: a -> Stack a ()
push x = Stack $ \stack ->
  ((), x : stack)

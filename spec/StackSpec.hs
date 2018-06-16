module StackSpec where

import Test.Hspec (Spec, shouldBe, describe, it)

import Data.Stack
import Data.Maybe (fromMaybe)

spec :: Spec
spec = do
  describe "Stack.nullStack" $ do
    it "does nothing to an empty stack" $ do
      execStack nullStack [] `shouldBe` ([] :: [()])

    it "does nothing to a populated stack" $ do
      execStack nullStack [0] `shouldBe` [0]

    it "returns () from an empty stack" $ do
      evalStack nullStack [] `shouldBe` ()

    it "returns () from a populated stack" $ do
      evalStack nullStack [0] `shouldBe` ()

  describe "Stack.runStack" $ do
    it "returns a result and new stack from a computation and stack" $ do
      runStack nullStack [] `shouldBe` ((), [] :: [()])

  describe "Stack.evalStack" $ do
    it "returns a result from a computation and stack" $ do
      evalStack nullStack [] `shouldBe` ()

  describe "Stack.execStack" $ do
    it "returns a new stack from a computation and stack" $ do
      execStack nullStack [] `shouldBe` ([] :: [()])

  describe "Stack.push" $ do
    it "pushes a value onto an empty stack" $ do
      execStack (push 0) [] `shouldBe` [0]

    it "pushes a value onto a populated stack head" $ do
      execStack (push 1) [0] `shouldBe` [1, 0]

    it "returns () when pushing onto an empty stack" $ do
      evalStack (push 0) [] `shouldBe` ()

    it "returns () when pushing onto a populated stack" $ do
      evalStack (push 1) [0] `shouldBe` ()

  describe "Stack.pop" $ do
    it "leaves an empty stack unchanged" $ do
      execStack pop [] `shouldBe` ([] :: [()])

    it "removes the head of a populated stack" $ do
      execStack pop [1] `shouldBe` []

    it "returns Nothing from an empty stack" $ do
      evalStack pop [] `shouldBe` (Nothing :: Maybe ())

    it "returns Just the head of a populated stack" $ do
      evalStack pop [1] `shouldBe` Just 1

  describe "a complex stack manipulation" $ do
    it "sets the stack to the expected final state" $ do
      execStack stackManip [] `shouldBe` [3, 0, 2, 1]

    it "returns the result of the final sequenced computation" $ do
      evalStack stackManip [] `shouldBe` ()

stackManip :: Stack Integer ()
stackManip = do
  a <- pop
  push 1
  push 2
  push 3
  b <- pop
  push (fromMaybe 0 a)
  push (fromMaybe 0 b)
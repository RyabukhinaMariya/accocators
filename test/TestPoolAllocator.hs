module TestPoolAllocator where

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Monad.ST (runST)
import PoolAllocator

testAllocateLimit :: H.Property
testAllocateLimit = H.property $ do
    n <- H.forAll $ Gen.int (Range.linear 1 100)
    let results = runST $ do
            pool <- createPool 1024 n
            sequence [allocateBlock pool | _ <- [1..n+1]]

    H.assert $ take n results == Prelude.map Just [0..n-1]
    H.assert $ last results == Nothing

testFunctionality :: H.Property
testFunctionality = H.property $ do
    let result = runST $ do
            pool <- createPool 1024 1
            _ <- allocateBlock pool
            deallocateBlock pool 0
            allocateBlock pool

    result H.=== Just 0
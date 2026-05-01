module TestFreeListAllocator where

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Monad.ST (runST)
import FreeListAllocator

testAllocateLimit :: H.Property
testAllocateLimit = H.property $ do
    n <- H.forAll $ Gen.int (Range.linear 1 1024)

    let (res1, res2) = runST $ do
            pool <- createPool n
            r1 <- allocateBlock pool (n)
            r2 <- allocateBlock pool 1
            return (r1, r2)

    res1 H.=== Just 0
    res2 H.=== Nothing

testDeallocateWithCoalesce :: H.Property
testDeallocateWithCoalesce = H.property $ do
    half <- H.forAll $ Gen.int (Range.linear 1 512)

    let n = half * 2
    let result = runST $ do
            pool <- createPool n
            b1 <- allocateBlock pool half
            b2 <- allocateBlock pool half

            case (b1, b2) of
                (Just a1, Just a2) -> do
                    deallocateBlock pool a1 half
                    deallocateBlock pool a2 half
                    allocateBlock pool n
                _ -> return Nothing
    result H.=== Just 0
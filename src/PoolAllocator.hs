module PoolAllocator where

import Control.Monad.ST
import Data.STRef

data PoolAllocator s = PoolAllocator {
    blockSize :: Int, 
    blockCount :: Int,
    freeBlocks :: STRef s [Int]
}

createPool :: Int -> Int -> ST s (PoolAllocator s)
createPool bSize bCount = do
    fBlocks <- newSTRef [0..bCount - 1]
    return $ PoolAllocator {
        blockSize = bSize,
        blockCount = bCount,
        freeBlocks = fBlocks
    }

allocateBlock :: PoolAllocator s -> ST s (Maybe Int)
allocateBlock allocator = do
    fBlocks <- readSTRef (freeBlocks allocator)
    case fBlocks of
        [] -> return Nothing
        (idx : rest) -> do
            writeSTRef (freeBlocks allocator) rest
            return (Just idx)

deallocateBlock :: PoolAllocator s -> Int -> ST s ()
deallocateBlock allocator idx = do
    modifySTRef' (freeBlocks allocator) (idx :)

deallocatePool :: PoolAllocator s -> ST s ()
deallocatePool allocator = do
    writeSTRef (freeBlocks allocator) [0 .. blockCount allocator - 1]
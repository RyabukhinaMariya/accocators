import Control.Monad.ST
import Data.Array.ST
import Data.STRef

data PoolAllocator s = PoolAllocator {
    blockSize :: Int, 
    blockCount :: Int,
    freeStack :: STRef s [Int]
}

createPool :: Int -> Int -> ST s (PoolAllocator s)
createPool blockSize blockCount = do
    freeBlocks <- newSTRef [0..blockCount - 1]
    return $ PoolAllocator {
        blockSize = blockSize,
        blockCount = blockCount,
        freeBlocks = freeBlocks
    }

allocateBlock :: PoolAllocator s -> ST s (Maybe Int)
allocateBlock allocator = do
    freeBlocks <- readSTRef (freeBlocks allocator)
    case freeBlocks of
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
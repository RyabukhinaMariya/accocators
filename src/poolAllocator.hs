import Control.Monad.ST
import Data.Array.ST
import Data.STRef

type FreeBlocks s = STRef s [Int]

data PoolAllocator s = PoolAllocator {
    blockSize :: Int, 
    blockCount :: Int,
    freeBlocks :: FreeBlocks s
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
    freeList <- readSTRef (freeBlocks allocator)
    case freeList of
        [] -> return Nothing
        (idx : rest) -> do
            writeSTRef (freeBlocks allocator) rest
            return (Just idx)

deallocateBlock :: PoolAllocator s -> Int -> ST s ()
deallocateBlock allocator idx = do
    modifySTRef' (freeBlocks allocator) (idx :)

deallocatePool :: PoolAllocator s -> ST s ()
deallocatePool allocator = do
    writeSTRef (freeBlocks allocator) [0..blockCount allocator - 1]

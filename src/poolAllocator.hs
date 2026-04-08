import Control.Monad.ST
import Data.Array.ST
import Data.STRef

data PoolAllocator a = PoolAllocator {
    blockSize :: Int, 
    blockCount :: Int,
    freeStack :: FreeBlocks a
}

createPool :: Int -> Int -> ST a (PoolAllocator a)
createPool blockSize blockCount = do
    freeBlocks <- newSTRef [0..blockCount - 1]
    return $ PoolAllocator {
        blockSize = blockSize,
        blockCount = blockCount,
        freeBlocks = freeBlocks
    }

allocateBlock :: PoolAllocator a -> ST a (Maybe Int)
allocateBlock allocator = do
    freeBlocks <- readSTRef (freeBlocks allocator)
    case freeBlocks of
        [] -> return Nothing
        (idx : rest) -> do
            writeSTRef (freeBlocks allocator) rest
            return (Just idx)

deallocateBlock :: PoolAllocator a -> Int -> ST a ()
deallocateBlock allocator idx = do
    modifySTRef' (freeBlocks allocator) (idx :)

deallocatePool :: PoolAllocator a -> ST a ()
deallaocatePool allocator = do
    writeSTRef (freeBlocks allocator) [0..blockCount allocator - 1]
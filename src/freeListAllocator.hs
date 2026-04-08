import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.List (sort)

data freeListAllocator s = PoolAllocator {
    poolSize   :: Int, 
    engageList :: STRef s [(Int, Int)] 
}

createPool :: Int -> ST s (freeListAllocator s)
createPool size = do
    list <- newSTRef []
    return $ PoolAllocator {
        poolSize = size,
        engageList = list
    }

allocateBlock :: freeListAllocator s -> Int -> ST s (Maybe Int)
allocateBlock allocator sizeReq = do
    engaged <- readSTRef (engageList allocator)
    let 
        findSuitable currentPos [] = 
            if (poolSize allocator) - currentPos >= sizeReq 
            then Just (currentPos, currentPos + sizeReq)
            else Nothing
        findSuitable currentPos ((start, end):rest) =
            if start - currentPos >= sizeReq
            then Just (currentPos, currentPos + sizeReq)
            else findSuitable end rest

    case findSuitable 0 engaged of
        Nothing -> return Nothing
        Just (newStart, newEnd) -> do
            writeSTRef (engageList allocator) (sort ((newStart, newEnd) : engaged))
            return $ Just newStart

deallocateBlock :: freeListAllocator s -> Int -> ST s ()
deallocateBlock allocator startPos = do
    engaged <- readSTRef (engageList allocator)
    let updated = filter (\(s, _) -> s /= startPos) engaged
    writeSTRef (engageList allocator) updated

deallocatePool :: freeListAllocator s -> ST s ()
deallocatePool allocator = do
    writeSTRef (engageList allocator) []
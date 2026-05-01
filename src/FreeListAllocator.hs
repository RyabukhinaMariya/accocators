module FreeListAllocator where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.ST
import Data.STRef
import Data.List (delete)
import Data.Function ((&))

-- freeTree - Size Balansed Tree, для быстрого поиска блока нужного размера
-- freeSet - множество, для поиска соседних свободных блоков
data FreeListAllocator s = FreeListAllocator {
    freeTree :: STRef s (Map.Map Int [Int]),   -- Размер -> [Адреса]
    freeSet  :: STRef s (Set.Set (Int, Int))   -- (Адрес, Размер)
}

createPool :: Int -> ST s (FreeListAllocator s)
createPool size = do
    let initTree = Map.singleton size [0]
    let initSet  = Set.singleton (0, size)
    treeRef <- newSTRef initTree
    setRef  <- newSTRef initSet
    return $ FreeListAllocator treeRef setRef

removeFromTree :: Int -> Int -> Map.Map Int [Int] -> Map.Map Int [Int]
removeFromTree size addr m =
    case Map.lookup size m of
        Nothing -> m
        Just addresses -> 
            let remainingAddrs = delete addr addresses
            in if null remainingAddrs
               then Map.delete size m
               else Map.insert size remainingAddrs m

allocateBlock :: FreeListAllocator s -> Int -> ST s (Maybe Int)
allocateBlock alloc reqSize = do
    tree <- readSTRef (freeTree alloc)
    set <- readSTRef (freeSet alloc)

    case Map.lookupGE reqSize tree of
        Nothing -> return Nothing
        Just (foundSize, targetAddr : remainingAddrs) -> do
            
            -- Удаление блока из обеих структур
            let newTree = removeFromTree foundSize targetAddr tree
            let newSet  = Set.delete (targetAddr, foundSize) set

            -- Подсчет остатка свободного места (удаленный блок может быть больше необходимого размера блока)
            let diff = foundSize - reqSize
            if diff > 0
                then do
                    let newAddr = targetAddr + reqSize
                    writeSTRef (freeTree alloc) (Map.insertWith (++) diff [newAddr] newTree)
                    writeSTRef (freeSet alloc)  (Set.insert (newAddr, diff) newSet)
                    return (Just targetAddr)
                else do
                    writeSTRef (freeTree alloc) newTree
                    writeSTRef (freeSet alloc)  newSet
                    return (Just targetAddr)
            return (Just targetAddr)


-- Освобождение блока
deallocateBlock :: FreeListAllocator s -> Int -> Int -> ST s ()
deallocateBlock alloc addr deallocSize = do
    tree <- readSTRef (freeTree alloc)
    set <- readSTRef (freeSet alloc)

    let leftNeighbor  = Set.lookupLT (addr, 0) set
    let rightNeighbor = Set.lookupGT (addr, 0) set

    let canMergeLeft = case leftNeighbor of
            Just (lAddr, lSize) -> lAddr + lSize == addr
            _ -> False
    
    let canMergeRight = case rightNeighbor of
            Just (rAddr, rSize) -> addr + deallocSize == rAddr
            _ -> False

    let finalAddr = if canMergeLeft 
                    then fst (maybe (0,0) id leftNeighbor) 
                    else addr
    
    let finalSize = deallocSize 
                  + (if canMergeLeft  then snd (maybe (0,0) id leftNeighbor)  else 0)
                  + (if canMergeRight then snd (maybe (0,0) id rightNeighbor) else 0)

    let newSet = set
            & (if canMergeLeft  then Set.delete (maybe (0,0) id leftNeighbor)  else id)
            & (if canMergeRight then Set.delete (maybe (0,0) id rightNeighbor) else id)
            & Set.insert (finalAddr, finalSize)

    let newTree = tree
            & (if canMergeLeft  then removeFromTree (snd $ maybe (0,0) id leftNeighbor) (fst $ maybe (0,0) id leftNeighbor) else id)
            & (if canMergeRight then removeFromTree (snd $ maybe (0,0) id rightNeighbor) (fst $ maybe (0,0) id rightNeighbor) else id)
            & Map.insertWith (++) finalSize [finalAddr]

    writeSTRef (freeSet alloc) newSet
    writeSTRef (freeTree alloc) newTree

freePool :: FreeListAllocator s -> Int -> ST s ()
freePool alloc size = do
    writeSTRef (freeTree alloc) (Map.singleton size [0])
    writeSTRef (freeSet alloc)  (Set.singleton (0, size))
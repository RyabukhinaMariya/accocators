# Allocators

This repository contains pool and free-list allocators implementation on haskell

## API Reference

### Pool Allocator

#### createPool

```haskell
  createPool :: Int -> Int -> ST s (PoolAllocator s)
```

| Parameter | Type     | Description                |
| :-------- | :------- | :------------------------- |
| `bSize` | `Int` | Size of one block |
| `bCount` | `Int` | Number of blocks allocated |

#### allocateBlock

```haskell
  allocateBlock :: PoolAllocator s -> ST s (Maybe Int)
```

| Parameter | Type     | Description                |
| :-------- | :------- | :------------------------- |
| `allocator` | `PoolAllocator s` | Pool Allocator |

#### deallocateBlock

```haskell
deallocateBlock :: PoolAllocator s -> Int -> ST s ()
```

| Parameter | Type     | Description                |
| :-------- | :------- | :------------------------- |
| `allocator` | `PoolAllocator s` | Pool Allocator |
| `idx` | `Int` | Index of block to be deleted |

#### deallocatePool

```haskell
  deallocatePool :: PoolAllocator s -> ST s ()
```

| Parameter | Type     | Description                |
| :-------- | :------- | :------------------------- |
| `allocator` | `PoolAllocator s` | Pool Allocator |

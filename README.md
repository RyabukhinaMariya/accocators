# Allocators

This repository contains pool and free-list allocators implementation on haskell

## API Reference

### Pool Allocator:

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

### Free List Allocator:

#### createPool

```haskell
  createPool :: Int -> ST s (FreeListAllocator s)
```

| Parameter | Type     | Description                |
| :-------- | :------- | :------------------------- |
| `size` | `Int` | Size of pool |

#### allocateBlock

```haskell
  allocateBlock :: FreeListAllocator s -> Int -> ST s (Maybe Int)
```

| Parameter | Type     | Description                |
| :-------- | :------- | :------------------------- |
| `alloc` | `PoolAllocator s` | Free List Allocator |
| `reqSize` | `Int` | Requested block size |

#### deallocateBlock

```haskell
deallocateBlock :: FreeListAllocator s -> Int -> Int -> ST s ()
```

| Parameter | Type     | Description                |
| :-------- | :------- | :------------------------- |
| `alloc` | `PoolAllocator s` | Free List Allocator |
| `addr` | `Int` | Index of block to be deleted |
| `deallocSize` | `Int` | Size of block to be deallocated |

#### deallocatePool

```haskell
  deallocatePool :: FreeListAllocator s -> Int -> ST s ()
```

| Parameter | Type     | Description                |
| :-------- | :------- | :------------------------- |
| `alloc` | `PoolAllocator s` | Free List Allocator |
| `size` | `Int` | Size of pool to be deallocated |

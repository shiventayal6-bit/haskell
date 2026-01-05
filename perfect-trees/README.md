# Perfect Trees - Complete Solution

Haskell implementation with all commonly tested functions.

## Quick Start
```bash
cabal update
cabal build
cabal test
cabal run perfect-trees
```

## Interactive Development
```bash
# Start REPL
cabal repl

# In REPL:
> import PerfectTrees
> import ListUtils
> countLeaves binaryTree
4
> catMaybes [Just 1, Nothing, Just 3]
[1,3]
> transpose [[1,2],[3,4]]
[[1,3],[2,4]]
> partition even [1..6]
([2,4,6],[1,3,5])
```

## Functions Included

### ListUtils Module
- `catMaybes` - Extract Just values
- `transpose` - Matrix transpose
- `partition` - Split by predicate
- `replicate` - Repeat value
- `and, or, any, all` - Boolean operations
- `take, drop, splitAt` - List slicing
- `takeWhile, dropWhile` - Conditional slicing

### PerfectTrees Module
- Basic: `countLeaves`, `treeDepth`, `leaves`
- Validation: `isPerfect`, `allAtSameDepth`
- Construction: `buildPerfect`, `buildPerfectFrom`
- Sets: `leavesToSet`, `treeUnion`, `treeIntersection`
- Advanced: `mapLeaves`, `filterLeaves`, `partitionLeaves`
- Transformations: `transposeForest`, `catMaybeLeaves`, `zipTrees`

## Project Structure
```
perfect-trees/
├── src/
│   ├── ListUtils.hs      # List utilities
│   └── PerfectTrees.hs   # Tree operations
├── app/Main.hs           # Demo executable
├── test/Spec.hs          # HUnit tests
└── perfect-trees.cabal   # Build config
```

## Test Coverage

90+ tests covering:
- All list utility functions
- Tree construction and validation
- Set operations
- Advanced transformations
- Edge cases and integration tests

Run: `cabal test --test-show-details=direct`
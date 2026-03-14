\section{Implementation}
In this section, we present a minimal implementation of our DFUDS representation for trees and evaluate its performance against a standard pointer-based baseline.

\hide{
\begin{code}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.DeepSeq
import Control.Exception        (evaluate)
import Control.Monad.ST         (ST, runST)
import Data.Bits
import Data.Word                (Word64)

-- Storable vectors are used for the DFUDS bit representation so that
-- the data can be stored contiguously in memory and accessed efficiently
-- by rank/select operations.
import qualified Data.Vector.Storable        as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- Rank/Select support from the hw-rankselect library.
-- These structures enable constant-time navigation operations
-- over the DFUDS bitvector (e.g., locating parents, children,
-- or subtree boundaries).
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Select0
import HaskellWorks.Data.RankSelect.Poppy512
-- Tasty-bench is used to compare the pointer-based tree traversal
-- against the DFUDS-based representation.
-- The goal is to evaluate memory locality and traversal performance.
import Test.Tasty.Bench
import Text.Printf              (printf)
-- Used to measure the precise memory footprint of the structures
-- in order to compare succinct vs pointer-based representations.
import GHC.DataSize             (recursiveSize)
\end{code}
}

\subsection{Basic Tree Baseline}

We first define a standard, pointer-based binary tree to serve as our baseline. It is similar to a standard algebraic datatype definition, but we enforce strictness using bang patterns (\texttt{!}). By manually discarding Haskell's default lazy evaluation on the structural fields, we aim to prevent excessive thunk buildup during evaluation.

\begin{code}
data Tree a = Leaf | Node !a !(Tree a) !(Tree a)

foldlTree :: (b -> a -> b) -> b -> Tree a -> b
foldlTree f z Leaf           = z
foldlTree f z (Node v l r)   =
  let z1 = f z  v            
      z2 = foldlTree f z1 l  
  in  foldlTree f z2 r       
\end{code}

However, simply adding bang patterns might not be enough for accurate benchmarking. In Haskell, generating a large structure dynamically (e.g., millions of nodes) results in lazy thunks being created rather than fully allocated objects. If we were to benchmark the traversal directly, the measured time would incorrectly include the allocation and evaluation time of the tree itself. 

To prevent this, we instantiate the \texttt{NFData} (Normal Form Data) typeclass from the \texttt{deepseq} library. This allows our benchmarking framework to deeply force the entire tree into memory (Normal Form)\footnote{\url{https://hackage.haskell.org/package/deepseq}} \textit{before} the execution timer starts, ensuring we strictly measure the traversal algorithm and not the garbage collector or the tree initialization overhead.

\begin{code}
instance NFData a => NFData (Tree a) where
  rnf Leaf         = ()
  rnf (Node v l r) = rnf v `seq` rnf l `seq` rnf r

countNodes :: Tree a -> Int
countNodes Leaf = 1
countNodes (Node _ l r) = 1 + countNodes l + countNodes r
\end{code}

We also provide a generator function \texttt{mkTree}, which creates a perfectly balanced tree of depth $d$ which is \emph{strictly evalueated}:

\begin{code}
mkTree :: Int -> Int -> Tree Int
mkTree _ 0     = Leaf
mkTree !val d  = Node val (mkTree (val+1) (d-1)) (mkTree (val+2) (d-1))
\end{code}


\subsection{DFUDS Succinct Tree}
In our Haskell implementation, the disentanglement between topology and data is represented by two separate fields. The topology is encoded in \texttt{succStruct}, while the actual values are stored contiguously in \texttt{values}. For the bit-vector, we utilize \texttt{Poppy512} from the \texttt{hw-rankselect} library. \texttt{Poppy512} is a highly optimized, cache-friendly bit-vector, enabling $O(1)$ time complexity for \texttt{rank} and \texttt{select} queries.

\begin{code}
data DFUDSTree a = DFUDSTree
  { succStruct :: !Poppy512
  , values     :: !(U.Vector a)
  , nNodes     :: !Int
  }

instance NFData (DFUDSTree a) where
  rnf (DFUDSTree bits vals _) = rnf bits `seq` rnf vals
\end{code}

As a running example, consider the following small tree:
\begin{code}
example :: Tree Int
example =
  Node 609
    (Node 608 (Node 605 Leaf Leaf) Leaf)
    (Node 607 Leaf Leaf)
\end{code}

Its topology, in DFUDS form (with a leading artificial \texttt{1}), is encoded by the following bitstring:
\[
\texttt{1} \ \texttt{110} \ \texttt{100} \ \texttt{00}
\]
and the corresponding payload vector contains the node values in DFS pre-order:
\[
\texttt{[609, 608, 605, 607].}
\]

To illustrate how DFUDS is constructed, we first present a naive list-based encoder. This implementation is intentionally inefficient and only serves to convey the core idea on small examples.

\begin{code}
-- Naive DFUDS encoder for the topology, using a list of 0/1 bits.
getBits :: Tree a -> [Int]
getBits Leaf             = [0]
getBits (Node _ l r) =
  let d = degree l r
  in  replicate d 1 ++ [0] ++ getBits l ++ getBits r
  where
    degree Leaf Leaf                 = 0
    degree Leaf (Node _ _ _)         = 1
    degree (Node _ _ _) Leaf         = 1
    degree (Node _ _ _) (Node _ _ _) = 2
\end{code}

We then pack this list of bits into a \texttt{Poppy512} bitvector in a similarly naive way, by repeatedly updating an immutable \texttt{Vector Word64}. This is asymptotically suboptimal, but perfectly adequate for a beta.

\begin{code}
bitsToPoppy :: [Int] -> (Poppy512, Int)
bitsToPoppy bits =
  let nBits  = length bits
      nWords = (nBits + 63) `div` 64
      zeroVec :: VS.Vector Word64
      zeroVec = VS.replicate nWords 0

      setOne acc (i, b) =
        case b of
          1 ->
            let wIx  = i `div` 64
                bIx  = i .&. 63
                oldW = acc VS.! wIx
                newW = setBit oldW bIx
            in  acc VS.// [(wIx, newW)]
          _ -> acc

      packed = foldl setOne zeroVec (zip [0..] bits)
  in  (makePoppy512 packed, nBits)
\end{code}

The stored value are extracted in the same DFS pre-order as used by \texttt{getBits}. We define a tree fold and use it to obtain an $O(n)$ conversion to a list.

\begin{code}
toListDFS :: Tree a -> [a]
toListDFS t = reverse (foldlTree (\acc v -> v : acc) [] t)
\end{code}

We can now define a simple conversion function from a pointer-based tree to our \texttt{DFUDSTree}. It materializes the DFUDS topology as a list of bits, packs it into a \texttt{Poppy512} bitvector, and stores the payloads in an unboxed vector.

\begin{code}
toDFUDS :: U.Unbox a => Tree a -> DFUDSTree a
toDFUDS t =
  let bits           = getBits t
      vals           = toListDFS t
      (pop, bitCount) = bitsToPoppy bits
  in  DFUDSTree
        { succStruct = pop
        , values     = U.fromList vals
        , nNodes     = bitCount
        }
\end{code}

This \texttt{toDFUDS} function should be viewed as an executable specification rather than an efficient implementation. Our final version will construct the topology bitvector directly, without going through intermediate lists or repeated vector updates.

Because the data payload is fully decoupled from the tree structure and stored in a contiguous unboxed vector, operations that only inspect values (such as folding) become extremely efficient. Instead of recursively chasing pointers through the heap, a DFUDS fold reduces to a cache-friendly linear scan:

\begin{code}
foldlDFUDS :: U.Unbox a => (b -> a -> b) -> b -> DFUDSTree a -> b
foldlDFUDS f z tree = U.foldl' f z (values tree)
\end{code}


\subsection{Preliminary Benchmarks}
To compare traversal performance, we construct a binary tree of a user-specified depth using \texttt{mkTree}, and then convert this pointer-based tree into our succinct \texttt{DFUDSTree} representation. Reading the depth from standard input ensures that GHC cannot specialize or precompute the entire benchmark at compile time, so all work is performed at runtime.

We rely on \texttt{tasty-bench} to obtain stable runtime measurements. The benchmark uses the \texttt{env} function to separate data generation from the actual measurements: the tree and its DFUDS encoding are fully evaluated once and then reused across all benchmark runs for \texttt{evalBoxed}, \texttt{foldlTree}, and \texttt{foldlDFUDS}. This prevents the cost of constructing the input data from being folded into the reported timings and avoids aggressive compile-time constant folding optimizations by GHC. In addition, we use the \texttt{ghc-datasize} library to measure the precise heap footprint of both representations.

\begin{code}
main :: IO ()
main = do
  putStrLn "Depth?"
  depthStr <- getLine
  let depth =
        case reads depthStr of
          [(d, "")] -> d
          _         -> error "Please enter a valid integer depth."

  let setupData = do
        vTree <- evaluate . force $ mkTree 0 depth
        let dTree = toDFUDS vTree
        -- force DFUDS as well, so both structures are fully allocated
        vTree `deepseq` dTree `deepseq` return (vTree, dTree)

  (v, d) <- setupData

  if depth <= 14
    then do
      sizeV <- recursiveSize $!! v
      sizeD <- recursiveSize $!! d

      printf "Vanilla Tree Size: %.2f MB\n"
        (fromIntegral sizeV / (1024 * 1024) :: Double)
      printf "DFUDS Tree Size:   %.2f MB\n"
        (fromIntegral sizeD / (1024 * 1024) :: Double)
    else
      printf "Exact memory usage tracking disabled for large depths (ghc-datasize is too slow).\n"

  -- Force full evaluation BEFORE printing sums
  v `deepseq` d `deepseq` return ()

  printf "Sum result Vanilla:   %d\n" (foldlTree (+) 0 v)
  printf "Sum via DFUDS Foldl (should match Vanilla): %d\n" (foldlDFUDS (+) 0 d)

  defaultMain
    [ env (return (v, d)) $ \ ~(vanilla, dfuds) ->
        bgroup "Visit"
          [ bench "Vanilla Eval"
              $ nf (foldlTree (+) 0) vanilla
          , bench "DFUDS Foldl Array"
              $ nf (foldlDFUDS (+) 0) dfuds
          ]
    ]

\end{code}

Because Haskell is lazily evaluated, constructing a large tree such as \texttt{mkTree 0 depth} does not immediately allocate all nodes in memory. If we were to benchmark \texttt{foldlTree} or \texttt{foldlDFUDS} directly on these thunks, the reported time would conflate two costs: building the tree (and its DFUDS encoding) and actually traversing it. This would unfairly penalize the succinct representation, since its construction is more expensive than that of the pointer-based baseline.

To obtain meaningful traversal timings, we explicitly force both the vanilla tree and its DFUDS encoding into normal form before running the benchmarks, using \texttt{evaluate . force} and \texttt{deepseq}. In this way, the measured time isolates the cost of the traversal itself, excluding the one-off construction overhead of the underlying data structures.


For a balanced binary tree of depth 15, we obtain the following empirical results. The succinct DFUDS representation uses roughly half the memory of the vanilla pointer-based tree, while achieving an order-of-magnitude speedup in traversal time.

\begin{Verbatim}[commandchars=\\\{\}, frame=single, fontsize=\small]
Depth?
15
Vanilla Tree Size: 1.00 MB
DFUDS Tree Size:   0.52 MB
Sum result Vanilla:   638979
Sum via DFUDS Foldl: 638979

All
  Visit
    Vanilla Eval:      OK (0.93s)
      3.47 ms \(\pm\) 265 \(\upmu\)s
    DFUDS Foldl Array: OK (0.21s)
      47.6 \(\upmu\)s \(\pm\) 3.9 \(\upmu\)s

All 2 tests passed (1.14s)

\end{Verbatim}
More impressively, with a depth of 20, we get the following results, confirming that our implemetation, albeit suboptimal, is order of magnitudes faster in traversal time.

\begin{Verbatim}[commandchars=\\\{\}, frame=single, fontsize=\small]
Depth?
20
Exact memory usage tracking disabled for large depths (ghc-datasize is too slow).
Sum result Vanilla:   28311555
Sum via DFUDS Foldl (should match Vanilla): 28311555
All
  Visit
    Vanilla Eval:      OK (1.05s)
      485  ms \(\pm\)  25 ms
    DFUDS Foldl Array: OK (0.45s)
      1.26 ms \(\pm\)  71 \(\upmu\)s
All 2 tests passed (1.50s)
\end{Verbatim}
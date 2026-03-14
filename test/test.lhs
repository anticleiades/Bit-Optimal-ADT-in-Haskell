\section{Simple Tests}
\label{sec:simpletests}

We will use the \texttt{QuickCheck} library to automatically generate random test cases for our core operations. 

Concretely, we will use QuickCheck for two purposes:
\begin{itemize}
  \item \textbf{Fold order}: to verify that the DFUDS-based folds visit node payloads in the intended traversal order and produce the same result as the corresponding folds over the pointer-based tree.
  \item \textbf{Zipper navigation}: to check that our (succinct) zipper operations move correctly within the tree—e.g.\ navigating up, down, left, and right always lands on the expected node and preserves structural invariants.
\end{itemize}

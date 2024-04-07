#' @title gemmBatch
#' @description Multiplies rectangular matrices A and B in row and column 
#' parallel batches to compute C = A B or C = t(A) B or C = A t(B) on a GPU
#' @param A a "vclMatrix" containing batches of matrices
#' @param B a "vclMatrix" containing batches of matrices
#' @param C a "vclMatrix" containing batches of matrices
#' @param transposeABC a vector of 1 or 0, where 1 indicates transpose the corresponding matrices, e.g., c(0,0,0)
#' @param submatrixA a vector specifying the range of each submatrix of A, c(rowStart, nRowsSub, nRowsTotal, colStart, nColsSub, nColsTotal)
#' @param submatrixB a vector specifying the range of each submatrix of B, c(rowStart, nRowsSub, nRowsTotal, colStart, nColsSub, nColsTotal)
#' @param submatrixC a vector specifying the range of each submatrix of C, c(rowStart, nRowsSub, nRowsTotal, colStart, nColsSub, nColsTotal)
#' @param batches a vector containing parameters: 
#' c(nRowBatch, nColBatch, recycleArow, recycleAcol, recycleBrow, recycleBcol), 
#' where recycleArow = 1 indicates no row batches for A (use the same A for all batches)
#' 
#' @param Nglobal a vector of six numbers specifying number of global work items and local work items
#' @param NlocalCache a vector: c(cacheSizeA, cacheSizeB)
#' @param verbose a logical value, if TRUE, print extra information, default is FALSE
#' 
#' @return this function returns nothing, it modifies the input "vclMatrix" C in place.
#' @note computed results are stored in C, no objects are returned.
#' If A and B have different numbers of row batches, then one of their row batch numbers must be 1.
#' @useDynLib gpuBatchMatrix
#' @export



gemmBatch <- function(
  A, B, C, #vclmatrices
  transposeABC,
  submatrixA,
  submatrixB,
  submatrixC,
  batches, 
  Nglobal,
  NlocalCache,
  verbose=FALSE){
  
  

  if(missing(Nglobal)) {
    Nglobal <- c(64,8,2, 1, 4, 1)
  }
  
  Nglobal[4]=1
  
  if(verbose){ message(paste('global work items', Nglobal))}

  
  gemmBatch2backend(A,B,C,transposeABC,  
                    submatrixA,
                    submatrixB,
                    submatrixC, 
                    batches, 
                    Nglobal,   
                    NlocalCache,
                    verbose)
  
  }









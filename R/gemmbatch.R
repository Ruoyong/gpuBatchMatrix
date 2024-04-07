#' @title gemmBatch
#' @description Multiplies a rectangular matrix A by a rectangular matrix B in row and column parallel batches
#' @param A a "vclMatrix" consists of batches of matrices
#' @param B a "vclMatrix" consists of batches of matrices
#' @param C a "vclMatrix" consists of batches of matrices
#' @param transposeABC a vector of 1 or 0, 1 indicates transpose the matrices, e.g., c(0,0,0)
#' @param submatrixA a vector that indicates the range of each submatrix of A, c(rowStart, nRowsSub, nRowsTotal, colStart, nColsSub, nColsTotal)
#' @param submatrixB a vector that indicates the range of each submatrix of B, c(rowStart, nRowsSub, nRowsTotal, colStart, nColsSub, nColsTotal)
#' @param submatrixC a vector that indicates the range of each submatrix of C, c(rowStart, nRowsSub, nRowsTotal, colStart, nColsSub, nColsTotal)
#' @param batches a vector that contains c(nRowBatch, nColBatch, recycleArow, recycleAcol, recycleBrow, recycleBcol), recycleArow=1 indicates there are no row batches for A, use the same A for all batches
#' @param workgroupSize vector of six numbers, number of global work items and local work items,
#' @param NlocalCache a vector, c(cacheSizeA, cacheSizeB)
#' @param verbose a logical value, if TRUE, print extra information, default is FALSE
#' 
#' @return returns nothing, this function modifies the input "vclMatrix" C in place.
#' @note computed results are stored in C, no returned objects.
#' If A and B have different number of row batches, then one of them's row batch number must be 1.
#' @useDynLib gpuBatchMatrix
#' @export



gemmBatch <- function(
  A, B, C, #vclmatrices
  transposeABC,
  submatrixA,
  submatrixB,
  submatrixC,
  batches, 
  workgroupSize,
  NlocalCache,
  verbose=FALSE){
  
  

  if(missing(workgroupSize)) {
    workgroupSize <- c(64,8,2, 1, 4, 1)
  }
  
  workgroupSize[4]=1
  
  if(verbose){ message(paste('global work items', workgroupSize))}

  
  gemmBatch2backend(A,B,C,transposeABC,  
                    submatrixA,
                    submatrixB,
                    submatrixC, 
                    batches, 
                    workgroupSize,   
                    NlocalCache,
                    verbose)
  
  }









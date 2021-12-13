#' @title multiplyLowerDiagonalBatch
#' @description computes output = LDB in batches on a GPU
#' @param output the output of LDB
#' @param L  lower triangular matrices in batches
#' @param D  diagonal matrices in batches, each row contains diagonal elements of D' 
#' @param B  matrices in batches
#' @param diagIsOne logical, whether the diagonal of L is one 
#' @param transformD how to transform D, can be any OpenCL C built-in math function
#' @param Nglobal the size of the index space for use
#' @param Nlocal the work group size of the index space 
#' @param NlocalCache a number
#' @note computed results are stored in output, no returned objects
#' 
#' @useDynLib gpuBatchMatrix
#' @export






multiplyLowerDiagonalBatch <- function(
                      output, L, D, B, # output = L  D B, L lower triangular, D diagonal
                      diagIsOne, # diagonal of L is one
                      transformD, 
                      Nglobal,
                      Nlocal,
                      NlocalCache){
  

  
  multiplyLowerDiagonalBatchBackend(
               output,
               L,
               D,
               B,
               diagIsOne,
               transformD,
               Nglobal,
               Nlocal,
               NlocalCache)
  
  
  
}












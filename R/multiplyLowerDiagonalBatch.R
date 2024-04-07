#' @title multiplyLowerDiagonalBatch
#' @description Computes output = LD^T B in parallel batches on a GPU.
#' @param L  a `vclMatrix' consists of batches of lower triangular matrices.
#' @param D  a `vclMatrix' consists of diagonal matrices in batches, each row is one batch contains diagonal elements of D^T.
#' @param B  a `vclMatrix' consists of matrices in batches.
#' @param diagIsOne a logical value indicating whether the diagonal of L is one. 
#' @param transformD a character string specifying how to transform D, can be any OpenCL C built-in math function.
#' @param output a `vclMatrix' which is the result of LD'B.
#' @param Nglobal the size of the index space for use.
#' @param Nlocal the work group size of the index space 
#' @param NlocalCache an integer specifying amount of local memory to cache.
#' 
#' @return this function returns nothing, it modifies the input `vclMatrix' output in place.
#' @note the computed results are stored in `output'.
#' @useDynLib gpuBatchMatrix
#' @export






multiplyLowerDiagonalBatch <- function(L, D, B, output,# output = L  D B, L lower triangular, D diagonal
                                diagIsOne, # diagonal of L is one
                                transformD, 
                                Nglobal,
                                Nlocal,
                                NlocalCache){
  
  if(missing(output)) {
    output = vclMatrix(0, nrow(L), ncol(B), 
                    type = gpuR::typeof(L))
  }  
  
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
  
  invisible(output)
  
}












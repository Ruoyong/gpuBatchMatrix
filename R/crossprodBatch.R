#' @title crossprodBatch
#' @description Computes C = t(A) A or t(A) D' A or t(A) D'^(-1) A in parallel batches on a GPU.
#' @param C a vclMatrix containing square matrices batches.
#' @param A a vclMatrix containing rectangular matrix batches.
#' @param D a vclMatrix where rows are diagonals of D' stacked row-wise. 
#' @param invertD a logical value, if TRUE, C = t(A) D^(-1) A.
#' @param Nglobal a vector specifying number of global work items.
#' @param Nlocal a vector specifying number of local work items.
#' @param NlocalCache an integer specifying amount of local memory to cache.
#' @param Cstartend a vector (startrow, nRow, startcol,nCol) that specifying the range of each submatrix in C.
#' @param Astartend a vector (startrow, nRow, startcol,nCol) that specifying the range of each submatrix in A.
#' @param Dstartend a vector (startrow, nRow, startcol,nCol) that specifying the range of each submatrix in D.
#' 
#' @return this function returns nothing, it modifies the input vclMatrix C in place.
#' @note The computed results are stored in C, no objects are returned.
#' @useDynLib gpuBatchMatrix
#' @export


 
crossprodBatch <- function(C,   # must be batch of square matrices 
                           A,
                           D,
                           invertD,
                           Nglobal, 
                           Nlocal, 
                           NlocalCache,
                           Cstartend, Astartend, Dstartend) {
  
  Nbatches = nrow(C)/ncol(C)
  
  if(missing(Cstartend)) {
    Cstartend=c(0, ncol(C), 0, ncol(C))
  }
  
  if(missing(Astartend)) {
    Astartend=c(0, nrow(A)/Nbatches, 0, ncol(A))
  }
  
  if(missing(Dstartend)) {
    Dstartend=c(0, 1, 0, ncol(D))
  }
  
  
  if((NlocalCache - Nlocal[1]*Nlocal[2])<0){
    warning("a larger NlocalCache required")
  }
  
  crossprodBatchBackend(C,A,D,invertD,Cstartend,Astartend,Dstartend, Nglobal,Nlocal, NlocalCache)
  
  invisible()
  
  
}





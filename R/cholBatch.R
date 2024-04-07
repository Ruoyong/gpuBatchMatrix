#' @title cholBatch
#' @description Performs Cholesky decomposition in parallel batches on a GPU, solving A = L D' L^t 
#' @param A a vclMatrix containing positive definite matrices in batches
#' @param D a vclMatrix where each row contains the diagonal elements of D' 
#' @param numbatchD number of batches in A
#' @param Nglobal a vector specifying number of global index space
#' @param Nlocal a vector specifying number of local work items
#' @param NlocalCache an integer specifying amount of local memory to cache
#' @param Astartend a vector specifying the range of A, c(startrow, numberofrows, startcolumn, numberofcols), row starts from 0
#' @param Dstartend a vector specifying the range of D
#' @param verbose a logical value, if TRUE, print extra information, default is FALSE
#' 
#' @return this function returns nothing, it modifies the input `vclMatrix' A and D in place.
#' @note the computed L and D' are stored in A and D respectively, no objects are returned
#' @useDynLib gpuBatchMatrix
#' @import gpuR
#' @export



cholBatch <- function(A,
                      D,
                      numbatchD,
                      Nglobal,
                      Nlocal,   # needs Nglobal[2]=Nlocal[2]
                      NlocalCache = gpuR::gpuInfo()$localMem/32,
                      Astartend,
                      Dstartend,
                      verbose=FALSE){
  
  
  if(missing(Astartend) & missing(numbatchD) & missing(D)){
    Astartend = c(0, ncol(A), 0, ncol(A))
    numbatchD = nrow(A)/ncol(A)
    D = vclMatrix(0, numbatchD, ncol(A), type = gpuR::typeof(A))
  }
  
  if(missing(numbatchD) )
     numbatchD=nrow(D)
     
  if(missing(D)) 
    D = vclMatrix(0, numbatchD, ncol(A), type = gpuR::typeof(A))
  
  if(missing(Astartend)) 
    Astartend=c(0, nrow(A)/numbatchD, 0, ncol(A))
  
  if(missing(Dstartend)) {
    Dstartend=c(0, numbatchD, 0, ncol(D))
  }
  
  
   if(Nlocal[2]!=Nglobal[2]){
     warning("local and global work sizes should be identical for dimension 2, ignoring global")
     Nglobal[2]=Nlocal[2]
   }
  
 
  
  if(verbose){ message(paste('global work items', Nglobal,
                             'local work items', Nlocal))}
  
  
  
  
  cholBatchBackend(A, D, 
                   Astartend, Dstartend, 
                   numbatchD,
                   Nglobal, Nlocal, NlocalCache)
  
   #theResult = list(L=A, diag=D)


   #theResult
   invisible(D)
  
  
}
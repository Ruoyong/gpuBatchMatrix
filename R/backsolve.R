#' @title backsolveBatch
#' @description Solve A * C = B for C on a GPU, where A, B and C are batches of square matrices of class 'vclMatrix'.
#' @param C an object of class 'vclMatrix'
#' @param A an object of class 'vclMatrix', with upper triangular values 0
#' @param B an object of class 'vclMatrix', consists of batches of rectangular matrices.
#' @param numbatchB number of batches in B. If 1, B uses a single matrix for all matrix batches in A.
#' @param diagIsOne a logical value, if TRUE, all the diagonal entries in matrices in A are 1.
#' @param Nglobal Size of the index space for use.
#' @param Nlocal Work group size of the index space.
#' @param NlocalCache an integer specifying amount of local memory to cache.
#' @param Cstartend a vector that selects the range of C, c(startrow, numberofrows, startcolumn, numberofcols), row starts from 0.
#' @param Astartend a vector that selects the range of A.
#' @param Bstartend a vector that selects the range of B.
#' @param verbose a logical value, if TRUE, print extra information, default is FALSE.
#' 
#' @return returns nothing, this function modifies the input "vclMatrix" C in place.
#' @note result matrices are stored in C, no returned objects.
#' @useDynLib gpuBatchMatrix
#' @export



backsolveBatch <- function(C, 
                           A,  # must be batches of square matrices
                           B,  #vclmatrices
                           numbatchB, #sometimes B can have only 1 batch, for repeated same batches
                           diagIsOne,
                           Nglobal, 
                           Nlocal, 
                           NlocalCache,
                           Cstartend,
                           Astartend,
                           Bstartend,
                           verbose=FALSE){

  
  nbatch<-nrow(A)/ncol(A)

  if(missing(Cstartend)) {
    Cstartend=c(0, nrow(C)/nbatch, 0, ncol(C))
  }
  
  if(missing(Astartend)) {
    Astartend=c(0, nrow(A)/nbatch, 0, ncol(A))
  }
  
  if(missing(Bstartend)) {
    Bstartend=c(0, nrow(B)/numbatchB, 0, ncol(B))
   }
  

  
  
  
  
  
  if(verbose){ message(paste('global work items', Nglobal,
                             'local work items', Nlocal))}


 

  backsolveBatchBackend(C, A, B, 
                        Cstartend, Astartend, Bstartend, 
                        numbatchB, diagIsOne, 
                        Nglobal, Nlocal, NlocalCache)



  }

  
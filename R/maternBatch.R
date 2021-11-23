#' @title maternBatch 
#' @description computes matern covariance matrices in batches on a GPU
#' @param var a vclMatrix on GPU, consists of the output matern matrices in batches
#' @param coords a vclMatrix on GPU, rectangular matrix batches
#' @param param a vclMatrix on GPU, matrix of paramters in batches, each row is a set of parameters 
#' @param Nglobal vector of number of global work items
#' @param Nlocal vector of number of local work items
#' @param startrow starting row of parameter matrix
#' @param numberofrows number of rows for computation
#' @note computed results are stored in var, no returned objects
#' @useDynLib gpuBatchMatrix
#' @export



maternBatch <- function(param, #22 columns 
                        coords,
                        var,  # the output matern matrices
                        Nglobal,
                        Nlocal,
                        startrow,   # new added
                        numberofrows){
  
if('SpatialPoints' %in% names(coords)) {
  coords = vclMatrix(coords@coords, 
                     type = c('float','double')[1+gpuInfo()$double_support])
}
    
if(is.matrix(param)) {
  param = maternGpuParam(param,  type = gpuR::typeof(coords))
}  
  
  if(missing(startrow) | missing(numberofrows)) {
    startrow=0
    numberofrows=nrow(param)
  }
  
if(missing(var)) {
  var = vclMatrix(0, nrow(coords)*numberofrows, nrow(coords), 
                  type = gpuR::typeof(coords))
}  
  
  maternBatchBackend(var, coords, param,
                     Nglobal, Nlocal,
                     startrow, numberofrows)
  
  invisible(var)

}





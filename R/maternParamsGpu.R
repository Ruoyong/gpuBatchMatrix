#' @title maternGpuParam
#' @description Create parameters be used by maternBatch on GPU
#' @param x R matrix of covariance parameters 
#' @useDynLib gpuBatchMatrix
#' @export



maternGpuParam = function(x, type='double') {

  
  if (dim(x)[1]==1){
  names<-colnames(x)
  x = geostatsp::fillParam(x)
  x=matrix(x, nrow=1, ncol=length(x))
  paramsGpu = vclMatrix(cbind(x,
                              matrix(0, nrow(x), 22-ncol(x))),type=type)
  gpuR::colnames(paramsGpu) = c(names,"anisoAngleDegrees")
   }


  else if(dim(x)[1]>1){
  x = geostatsp::fillParam(x)
  paramsGpu = vclMatrix(cbind(x, matrix(0, nrow(x), 22-ncol(x))),type=type)
  gpuR::colnames(paramsGpu) = colnames(x)
  }
  
  paramsGpu
  
}






























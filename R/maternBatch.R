#' @title maternBatch 
#' @description Computes Matérn covariance matrices in parallel batches on a GPU.
#' @param var a vclMatrix of the output Matérn matrices in batches.
#' @param coords a vclMatrix of the coordinates on GPU.
#' @param param a vclMatrix containing batches of parameters, each row represents a set of parameters.
#' @param Nglobal a vector specifying number of global index space.
#' @param Nlocal a vector specifying number of local work items.
#' @param startrow an integer specifying the starting row of parameter matrix.
#' @param numberofrows an integer specifying number of rows of the param to use.
#' 
#' @return this function returns nothing, it modifies the input var in place.
#' @note the computed results are stored in `var'.
#' @useDynLib gpuBatchMatrix
#' @export



maternBatch <- function(param, #22 columns 
                        coords,
                        var,  # the output matern matrices
                        Nglobal,
                        Nlocal,
                        startrow,   # new added
                        numberofrows){
  
  if("SpatRaster" %in% class(coords)   ) {
    if(requireNamespace("terra")) {
      coords = terra::xyFromCell(coords, 1:terra::ncell(coords))
    } else {
      stop("install the raster package to use these coordinates")
    }
    
  }

if('SpatVector' %in% class(coords)) {
  coords = terra::crds(coords)
}
  if(is.matrix(coords)) {
  coords = vclMatrix(coords, 
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





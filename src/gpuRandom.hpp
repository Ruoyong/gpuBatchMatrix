//#pragma OPENCL EXTENSION cl_khr_fp64 : enable


#include <Rcpp.h>
#include <string>


#include <dynMatrix/dynVCLMatGeostatsgpu.hpp>
#include <dynMatrix/dynVCLVecGeostatsgpu.hpp>
#include "viennacl/linalg/sum.hpp"
#include "viennacl/ocl/backend.hpp"

//from typeDef.cpp
template <typename T> std::string openclTypeString();



//////////////////////////////////////////////////////////////////////////////////////////////////////
//#include "viennacl/linalg/lu.hpp"

extern "C" void Rtemme_gamma(double *nu, double * g_1pnu, double * g_1mnu, double *g1, double *g2);

//template<typename T> double luT(viennacl::matrix<T> &vclX, viennacl::vector_base<T> &vclD);

template <typename T> T maternClEpsilon();



















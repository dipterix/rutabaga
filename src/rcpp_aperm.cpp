// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

struct ArrayPerm : public Worker
{
  const Rcpp::NumericVector x;
  const Rcpp::IntegerVector dims;
  const Rcpp::IntegerVector new_dim;
  const Rcpp::IntegerVector perm;

  Rcpp::NumericVector y;

  // ArrayPerm aperm(x, dims, new_dim, perm, len, re);
  ArrayPerm(
    const Rcpp::NumericVector x,
    const Rcpp::IntegerVector dims,
    const Rcpp::IntegerVector new_dim,
    const Rcpp::IntegerVector perm,
    const Rcpp::NumericVector y
  ): x(x), dims(dims), new_dim(new_dim), perm(perm), y(y){}

  void operator()(std::size_t begin, std::size_t end) {
    /*
     * for i in seq(begin, end) do:
     *    y[i] = x[a,b,c,d] in which y[i] = y[a,b,d,c] // perm = c(1,2,4,3)
     */
    int *output_ind = new int[new_dim.length()];
    int *input_ind = new int[new_dim.length()];

    unsigned long int i,j,k,l;
    int a;    // indices

    for(i = begin; i < end; i++){

      // Calculate output index
      a = i;
      for(j = 0; j < new_dim.length(); j++){
        /*
        * i -> coord in y
        * coord_1 = i % dims[0]
        * res_1 = (i - coord_y1) / dims[keep[0]]
        * coord_y2 = res_1 % dims[keep[1]]
        * res_2 = (res_1 - coord_y2) / dims[keep[1]]
        * coord_y3 = res_2 / dims[keep[2]] % dims[keep[2]]
        * ...
        */
        output_ind[j] = a % new_dim[j];
        input_ind[perm[j] - 1] = output_ind[j];
        a = (int)(a / new_dim[j]);
      }

      l = 0;
      for(k = new_dim.length() - 1; k >= 0; k--){
        l += input_ind[k] + l * (dims[k] - 1);
      }

      /*
      * Replace output[i] with x[l];
      */
      y[i] = x[l];
    }

    delete [] input_ind;
    delete [] output_ind;

  }

};


// [[Rcpp::export]]
Rcpp::NumericVector arraypermer(
    Rcpp::NumericVector x, Rcpp::IntegerVector dims, Rcpp::IntegerVector perm)
{
  // Generate template output
  int len = 1;
  Rcpp::IntegerVector new_dim(perm.length());
  for(int i=0; i<perm.length(); i++){
    len *= dims[perm[i]-1];
    new_dim[i] = dims[perm[i]-1];
  }
  Rcpp::NumericVector re(len);


  ArrayPerm aperm(x, dims, new_dim, perm, re);

  parallelFor(0, len, aperm);

  return(re);
}

/*** R
# dat = array(1:16, c(4,4))
# re = arraypermer(dat, dim(dat), c(2,1))
# dim(re) = dim(dat)[c(2,1)]
# re
*/

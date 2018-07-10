#include <Rcpp.h>
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

struct Sum : public Worker
{
  // source vector
  Rcpp::NumericVector x;
  const Rcpp::NumericVector y;
  const double a;

  // constructors
  Sum(
    Rcpp::NumericVector x,
    const Rcpp::NumericVector y,
    double a
  ) : x(x), y(y), a(a) {}

  // accumulate just the element of the range I've been asked to
  void operator()(std::size_t begin, std::size_t end) {
    int y_len = y.length();
    for(int i = begin; i < end; i++){
      x[i] = x[i] + y[i % y_len] * a;
    }
  }

};

// [[Rcpp::export]]
Rcpp::NumericVector vec_sum(Rcpp::NumericVector x, Rcpp::NumericVector y, double a) {

  // declare the SumBody instance
  Sum sum(x, y, a);

  // call parallel_reduce to start the work
  parallelFor(0, x.length(), sum);

  // return the computed sum
  return x;
}


/*** R
x = 1:10
vec_sum(x,x,1)

vec = force(rnorm(300*300*100*1))
microbenchmark::microbenchmark(
  vec + vec,
  vec_sum(vec,vec,1),
  times = 10L
)
*/

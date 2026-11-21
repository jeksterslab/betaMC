// -----------------------------------------------------------------------------
// edit .setup/cpp/linearAlgebra-vech-dot.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//' Half-Vectorization of a Square Matrix (C++)
//'
//' Stacks the lower triangle of a square matrix,
//' diagonal included, column by column, as `.Vech()` in R.
//' Called from other C++ functions; not exported to R.
//'
//' @author Ivan Jacob Agaloos Pesigan
//'
//' @param x Numeric \eqn{k \times k} matrix.
//'
//' @return Numeric row vector of length \eqn{k (k + 1) / 2}.
//'
//' @examples
//' # C++ only: VechCpp(x = sigma)
//'
//' @noRd
arma::rowvec VechCpp(const arma::mat& x) {
  const arma::uword k = x.n_rows;
  arma::rowvec out(k * (k + 1) / 2);
  arma::uword idx = 0;
  for (arma::uword j = 0; j < k; j++) {
    for (arma::uword i = j; i < k; i++) {
      out(idx) = x(i, j);
      idx++;
    }
  }
  return out;
}

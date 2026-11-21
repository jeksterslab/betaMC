// -----------------------------------------------------------------------------
// edit .setup/cpp/linearAlgebra-sym-of-vech-dot.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//' Symmetric Matrix from Its Half-Vectorization (C++)
//'
//' Builds the symmetric \eqn{k \times k} matrix
//' from its half-vectorization
//' (the lower triangle, column by column, as `.Vech()` in R).
//' Called from other C++ functions; not exported to R.
//'
//' @author Ivan Jacob Agaloos Pesigan
//'
//' @param x Numeric row vector of length \eqn{k (k + 1) / 2}.
//' @param k Positive integer. Dimension of the matrix.
//'
//' @return Numeric \eqn{k \times k} symmetric matrix.
//'
//' @examples
//' # C++ only: SymofVechCpp(x = vech_row, k = 3)
//'
//' @noRd
arma::mat SymofVechCpp(const arma::rowvec& x, const arma::uword& k) {
  arma::mat out(k, k);
  arma::uword idx = 0;
  for (arma::uword j = 0; j < k; j++) {
    for (arma::uword i = j; i < k; i++) {
      out(i, j) = x(idx);
      out(j, i) = x(idx);
      idx++;
    }
  }
  return out;
}

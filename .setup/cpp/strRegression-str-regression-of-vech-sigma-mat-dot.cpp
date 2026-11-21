// -----------------------------------------------------------------------------
// edit .setup/cpp/strRegression-str-regression-of-vech-sigma-mat-dot.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//' Regression Effect Sizes of Draws of the Covariance Matrix (C++)
//'
//' Regression effect sizes for each row of
//' \eqn{\operatorname{vech} \left( \boldsymbol{\Sigma} \right)},
//' where \eqn{\boldsymbol{\Sigma}} is the covariance matrix of
//' \eqn{\left( y, x_{1}, \dots, x_{p} \right)}:
//' standardized slopes (`betastar`), R-squared (`rsq`),
//' semipartial correlations (`spcor`),
//' and squared partial correlations (`pcorsq`).
//' Same quantities as `.BetaStarofSigma()`, `.RSqofSigma()`, `.SPCor()`,
//' and `.PCorSq()` in R.
//' Rows that fail (a nonpositive variance or a singular matrix) are `NA`.
//'
//' @author Ivan Jacob Agaloos Pesigan
//'
//' @param vechsigmacap Numeric matrix.
//'   Each row is the half-vectorization of a covariance matrix
//'   with `k * (k + 1) / 2` columns.
//' @param k Positive integer.
//'   Number of variables, \eqn{p + 1}.
//'
//' @return A list with the matrix `betastar` (`n` by `p`),
//'   the vector `rsq` (length `n`),
//'   and the matrices `spcor` and `pcorsq` (`n` by `p`).
//'
//' @examples
//' sigmacap <- matrix(data = c(4, 1, 1, 1, 9, 2, 1, 2, 16), nrow = 3)
//' vechsigmacap <- rbind(sigmacap[lower.tri(x = sigmacap, diag = TRUE)])
//' .StrRegressionofVechSigmaMat(vechsigmacap = vechsigmacap, k = 3)
//'
//' @noRd
// [[Rcpp::export(.StrRegressionofVechSigmaMat)]]
Rcpp::List StrRegressionofVechSigmaMat(const arma::mat& vechsigmacap,
                                       const arma::uword& k) {
  const arma::uword n = vechsigmacap.n_rows;
  const arma::uword p = k - 1;
  arma::mat betastar(n, p);
  arma::vec rsq(n);
  arma::mat spcor(n, p);
  arma::mat pcorsq(n, p);
  for (arma::uword r = 0; r < n; r++) {
    const arma::mat sigmacap = SymofVechCpp(vechsigmacap.row(r), k);
    const arma::mat sigmacapx = sigmacap.submat(1, 1, p, p);
    const arma::vec sigmayx = sigmacap.submat(1, 0, p, 0);
    const double sigmaysq = sigmacap(0, 0);
    arma::vec beta;
    bool ok =
        sigmaysq > 0 && arma::all(sigmacapx.diag() > 0) &&
        arma::solve(beta, sigmacapx, sigmayx, arma::solve_opts::likely_sympd);
    arma::mat invrhocapx;
    arma::vec sigmax;
    if (ok) {
      sigmax = arma::sqrt(sigmacapx.diag());
      const arma::mat rhocapx = sigmacapx / (sigmax * sigmax.t());
      ok = arma::inv_sympd(invrhocapx, rhocapx);
    }
    if (ok) {
      const double sigmay = std::sqrt(sigmaysq);
      const arma::vec b = beta % sigmax / sigmay;
      const double r2 = arma::dot(sigmayx, beta) / sigmaysq;
      const arma::vec sr = b % arma::sqrt(1.0 / invrhocapx.diag());
      const arma::vec srsq = arma::square(sr);
      betastar.row(r) = b.t();
      rsq(r) = r2;
      spcor.row(r) = sr.t();
      pcorsq.row(r) = (srsq / (1.0 - (r2 - srsq))).t();
    } else {
      betastar.row(r).fill(arma::datum::nan);
      rsq(r) = arma::datum::nan;
      spcor.row(r).fill(arma::datum::nan);
      pcorsq.row(r).fill(arma::datum::nan);
    }
  }
  return Rcpp::List::create(
      Rcpp::Named("betastar") = betastar, Rcpp::Named("rsq") = rsq,
      Rcpp::Named("spcor") = spcor, Rcpp::Named("pcorsq") = pcorsq);
}

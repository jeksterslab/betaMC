// -----------------------------------------------------------------------------
// edit .setup/cpp/processLM-mc-vech-sigma-lm-dot.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// vech of the model-implied covariance matrix of (y, x)
// for each Monte Carlo draw of theta (rows).
// Rows that are not admissible are NA.
// [[Rcpp::export(.MCVechSigmaLM)]]
arma::mat MCVechSigmaLM(const arma::mat& thetahatstar, const arma::uword& p) {
  const arma::uword n = thetahatstar.n_rows;
  const arma::uword k = p + 1;
  arma::mat out(n, k * (k + 1) / 2);
  for (arma::uword r = 0; r < n; r++) {
    bool ok;
    const arma::mat sigmacap = SigmaofThetaLMCpp(thetahatstar.row(r), p, ok);
    if (ok) {
      out.row(r) = VechCpp(sigmacap);
    } else {
      out.row(r).fill(arma::datum::nan);
    }
  }
  return out;
}

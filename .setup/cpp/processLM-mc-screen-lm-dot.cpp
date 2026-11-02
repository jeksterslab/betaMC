// -----------------------------------------------------------------------------
// edit functionsCpp/processLM/processLM-mc-screen-lm-dot.cpp
// (linked into .setup/cpp/; change it in the functionsCpp project)
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

arma::mat SymofVechCpp(const arma::rowvec& x, const arma::uword k);

// Model-implied covariance matrix of (y, x) from
// theta = (beta, sigmasq, vech(sigmacapx)).
// Returns false in ok when the draw is not admissible.
arma::mat SigmaofThetaLMCpp(const arma::rowvec& theta, const arma::uword p,
                            bool& ok) {
  const arma::uword k = p + 1;
  const arma::vec beta = theta.subvec(0, p - 1).t();
  const double sigmasq = theta(p);
  const arma::mat sigmacapx =
      SymofVechCpp(theta.subvec(k, theta.n_elem - 1), p);
  arma::mat sigmacap(k, k, arma::fill::zeros);
  ok = false;
  if (sigmasq > 0 && arma::all(sigmacapx.diag() > 0)) {
    const arma::vec sigmayx = sigmacapx * beta;
    const double sigmaysq = sigmasq + arma::dot(beta, sigmayx);
    if (sigmaysq > 0) {
      sigmacap(0, 0) = sigmaysq;
      sigmacap.submat(1, 0, p, 0) = sigmayx;
      sigmacap.submat(0, 1, 0, p) = sigmayx.t();
      sigmacap.submat(1, 1, p, p) = sigmacapx;
      arma::mat chol_sigmacap;
      ok = arma::chol(chol_sigmacap, sigmacap);
    }
  }
  return sigmacap;
}

// Admissibility of each Monte Carlo draw of theta (rows):
// sigmasq > 0, positive variances of x, sigmaysq > 0,
// and a positive definite covariance matrix of (y, x).
// Same checks as .MCThetaHat() in betaMC.
// [[Rcpp::export(.MCScreenLM)]]
Rcpp::LogicalVector MCScreenLM(const arma::mat& thetahatstar,
                               const arma::uword p) {
  const arma::uword n = thetahatstar.n_rows;
  Rcpp::LogicalVector out(n);
  for (arma::uword r = 0; r < n; r++) {
    bool ok;
    SigmaofThetaLMCpp(thetahatstar.row(r), p, ok);
    out[r] = ok;
  }
  return out;
}

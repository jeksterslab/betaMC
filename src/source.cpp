// -----------------------------------------------------------------------------
// edit functionsCpp/linearAlgebra/linearAlgebra-sym-of-vech-dot.cpp
// (linked into .setup/cpp/; change it in the functionsCpp project)
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// Symmetric k x k matrix from its half-vectorization
// (lower triangle, column by column, as .Vech() in R).
arma::mat SymofVechCpp(const arma::rowvec& x, const arma::uword k) {
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

// Half-vectorization of a square matrix
// (lower triangle, column by column, as .Vech() in R).
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
// -----------------------------------------------------------------------------
// edit functionsCpp/processLM/processLM-mc-vech-sigma-lm-dot.cpp
// (linked into .setup/cpp/; change it in the functionsCpp project)
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

arma::rowvec VechCpp(const arma::mat& x);
arma::mat SigmaofThetaLMCpp(const arma::rowvec& theta, const arma::uword p,
                            bool& ok);

// vech of the model-implied covariance matrix of (y, x)
// for each Monte Carlo draw of theta (rows).
// Rows that are not admissible are NA.
// [[Rcpp::export(.MCVechSigmaLM)]]
arma::mat MCVechSigmaLM(const arma::mat& thetahatstar, const arma::uword p) {
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
// -----------------------------------------------------------------------------
// edit
// functions/strRegression/strRegression-str-regression-of-vech-sigma-mat-dot.cpp
// (linked into .setup/cpp/; change it in the functionsCpp project)
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

arma::mat SymofVechCpp(const arma::rowvec& x, const arma::uword k);

// Regression effect sizes for each row of vech(sigmacap),
// where sigmacap is the covariance matrix of (y, x_1, ..., x_p):
// standardized slopes (betastar), R-squared (rsq),
// semipartial correlations (spcor),
// and squared partial correlations (pcorsq).
// Same quantities as .BetaStarofSigma(), .RSqofSigma(), .SPCor(),
// and .PCorSq() in R. Rows that fail are NA.
// [[Rcpp::export(.StrRegressionofVechSigmaMat)]]
Rcpp::List StrRegressionofVechSigmaMat(const arma::mat& vechsigmacap,
                                       const arma::uword k) {
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

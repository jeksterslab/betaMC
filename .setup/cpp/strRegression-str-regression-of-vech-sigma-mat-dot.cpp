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

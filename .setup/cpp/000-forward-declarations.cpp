// -----------------------------------------------------------------------------
// edit .setup/cpp/000-forward-declarations.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

Rcpp::LogicalVector MCScreenLM(const arma::mat& thetahatstar,
                               const arma::uword& p);

arma::mat MCVechSigmaLM(const arma::mat& thetahatstar, const arma::uword& p);

Rcpp::NumericVector Quantile(const arma::vec& x, const arma::vec& probs);

arma::mat SigmaofThetaLMCpp(const arma::rowvec& theta, const arma::uword& p,
                            bool& ok);

arma::mat SymofVechCpp(const arma::rowvec& x, const arma::uword& k);

Rcpp::List StrRegressionofVechSigmaMat(const arma::mat& vechsigmacap,
                                       const arma::uword& k);

arma::rowvec VechCpp(const arma::mat& x);

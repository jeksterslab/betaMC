% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betaMC-delta-r-sq-mc.R
\name{DeltaRSqMC}
\alias{DeltaRSqMC}
\title{Estimate Improvement in R-Squared
and Generate the Corresponding Sampling Distribution
Using the Monte Carlo Method}
\usage{
DeltaRSqMC(object)
}
\arguments{
\item{object}{Object of class \code{mc}, that is,
the output of the \code{MC()} function.}
}
\value{
Returns an object
of class \code{betamc} which is a list with the following elements:
\describe{
\item{object}{The function argument \code{object}.}
\item{thetahatstar}{Sampling distribution of
\eqn{\Delta R^{2}}.}
\item{vcov}{Sampling variance-covariance matrix of
\eqn{\Delta R^{2}}.}
\item{est}{Vector of estimated
\eqn{\Delta R^{2}}.}
\item{fun}{Function used ("DeltaRSqMC").}
}
}
\description{
Estimate Improvement in R-Squared
and Generate the Corresponding Sampling Distribution
Using the Monte Carlo Method
}
\details{
The vector of improvement in R-squared
(\eqn{\Delta R^{2}})
is derived from each randomly generated vector of parameter estimates.
Confidence intervals are generated by obtaining
percentiles corresponding to \eqn{100(1 - \alpha)\%}
from the generated sampling
distribution of \eqn{\Delta R^{2}},
where \eqn{\alpha} is the significance level.
}
\examples{
# Fit the regression model
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
# Generate the sampling distribution of parameter estimates
# (use a large R, for example, R = 20000 for actual research)
mc <- MC(object, R = 100)
# Generate confidence intervals for improvement in R-squared
deltarsq <- DeltaRSqMC(mc)
# Methods --------------------------------------------------------
print(deltarsq)
summary(deltarsq)
coef(deltarsq)
vcov(deltarsq)
confint(deltarsq, level = 0.95)
}
\seealso{
Other Beta Monte Carlo Functions: 
\code{\link{BetaMC}()},
\code{\link{DiffBetaMC}()},
\code{\link{MC}()},
\code{\link{PCorMC}()},
\code{\link{RSqMC}()},
\code{\link{SCorMC}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{Beta Monte Carlo Functions}
\keyword{betaMC}
\keyword{deltarsq}

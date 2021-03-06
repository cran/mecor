% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ipwm.R
\name{ipwm}
\alias{ipwm}
\title{Weighting for Confounding and Joint Misclassification of Exposure and Outcome}
\usage{
ipwm(
  formulas,
  data,
  outcome_true,
  outcome_mis = NULL,
  exposure_true,
  exposure_mis = NULL,
  nboot = 1000,
  conf_level = 0.95,
  fix_nNAs = FALSE,
  semiparametric = FALSE,
  optim_args = list(method = "BFGS"),
  force_optim = FALSE,
  sp = Inf,
  print = TRUE
)
}
\arguments{
\item{formulas}{a list of objects of \link[base]{class} \code{\link[stats]{formula}} specifying the probability models for the stats::terms of some factorisation of the joint conditional probability function of \code{exposure_true}, \code{exposure_mis}, \code{outcome_true} and \code{outcome_mis}, given covariates}

\item{data}{\code{\link[base]{data.frame}} containing \code{exposure.true}, \code{exposure.mis}, \code{outcome.true}, \code{outcome.mis} and covariates. Missings (\code{NA}s) are allowed on variables \code{exposure_true} and \code{outcome_true}.}

\item{outcome_true}{a character string specifying the name of the true outcome variable that is free of misclassification but possibly unknown (\code{NA}) for some (but not all) subjects}

\item{outcome_mis}{a character string specifying the name of the counterpart of \code{outcome_true} that is available on all subjects but potentially misclassifies subjects' outcomes. The default (\code{outcome_mis = NULL}) indicates absence of outcome misclassification}

\item{exposure_true}{a character string specifying the name of the true exposure variable that is free of misclassification but possibly unknown (\code{NA}) for some (but not all) subjects}

\item{exposure_mis}{a character string specifying the name of the counterpart of \code{exposure_true} that is available on all subjects but potentially misclassifies subjects as exposed or as non-exposed. The default (\code{exposure_mis = NULL}) indicates absence of exposure misclassification}

\item{nboot}{number of bootstrap samples. Setting \code{nboot == 0} results in point estimation only.}

\item{conf_level}{the desired confidence level of the confidence interval}

\item{fix_nNAs}{logical indicator specifying whether or not to fix the joint distribution of \code{is.na(exposure_true)} and \code{is.na(outcome_true)}. If \code{TRUE}, stratified bootstrap sampling is done according to the missing data pattern.}

\item{semiparametric}{logical indicator specifying whether or not to parametrically sample \code{exposure_true}, \code{exposure_mis}, \code{outcome_true} and \code{outcome_mis}. If \code{semiparametric == TRUE}, it is assumed that the missing data pattern is conditionally independent of these variables given covariates. Provided \code{nboot > 0}, the missing data pattern and covariates are sampled nonparametrically. \code{semiparametric} is ignored if \code{nboot == 0}.}

\item{optim_args}{arguments passed onto \code{\link[stats]{optim}} if called. See Details below for more information.}

\item{force_optim}{logical indicator specifying whether or not to force the \code{\link[stats]{optim}} function to be called}

\item{sp}{scalar shrinkage parameter in the interval \code{(0, Inf)}. Values closer to zero result in greater shrinkage of the estimated odds ratio to unity; \code{sp == Inf} results in no shrinkage.}

\item{print}{logical indicator specifying whether or not to print the output.}
}
\value{
\code{ipwm} returns an object of \link[base]{class} \code{ipwm}.
The returned object is a list containing the following elements:

\item{logOR}{the estimated log odds ratio;}
\item{call}{the matched function call.}

If \code{nboot != 0}, the list also contains

\item{SE}{a bootstrap estimate of the standard error for the estimator of the log odds ratio;}
\item{CI}{a bootstrap percentile confidence interval for the log odds ratio.}
}
\description{
\code{ipwm} implements a method for estimating the marginal causal odds ratio by constructing weights (modified inverse probability weights) that address both confounding and joint misclassification of exposure and outcome.
}
\details{
This function is an implementation of the weighting method described by Penning de Vries et al. (2018).
The method defaults to the estimator proposed by Gravel and Platt (2018) in the absence of exposure misclassification.

The function assumes that the exposure or the outcome has a misclassified version. An error is issued when both \code{outcome_mis} and \code{exposure_mis} are set to \code{NULL}.

Provided \code{force_optim = FALSE}, \code{ipwm} is considerably more efficient when the \code{\link[stats]{optim}} function is not invoked; i.e., when (1) \code{exposure_mis = NULL} and the formula for \code{outcome_true} does not contain stats::terms involving \code{outcome_mis} or \code{exposure_true}, (2) \code{outcome_mis = NULL} and the formula for \code{exposure_true} does not contain stats::terms involving \code{exposure_mis} or \code{outcome_true}, or (3) \code{all(is.na(data[, exposure_true]) == is.na(data[, outcome_true]))} and the formulas for \code{exposure_true} and \code{outcome_true} do not contain stats::terms involving \code{exposure_mis} or \code{outcome_mis}. In these cases, \code{ipwm} uses iteratively reweighted least squares via the \code{\link[stats]{glm}} function for maximum likelihood estimation. In all other cases, \code{optim_args} is passed on to \code{\link[stats]{optim}} for optimisation of the joint likelihood of \code{outcome_true}, \code{outcome_mis}, \code{exposure_true} and \code{exposure_mis}.
}
\examples{
data(sim) # simulated data on 10 covariates, exposure A and outcome Y.
formulas <- list(
  Y ~ A + L1 + L2 + L3 + L4 + L5 + L6 + L7 + L8 + L9 + L10 + B + Z,
  A ~ L1 + L2 + L3 + L4 + L5 + L6 + L7 + L8 + L9 + L10 + B + Z,
  Z ~ L1 + L2 + L3 + L4 + L5 + L6 + L7 + L8 + L9 + L10 + B,
  B ~ L1 + L2 + L3 + L4 + L5 + L6 + L7 + L8 + L9 + L10
)
\dontrun{
ipwm_out <- ipwm(
  formulas = formulas,
  data = sim,
  outcome_true = "Y",
  outcome_mis = "Z",
  exposure_true = "A",
  exposure_mis = "B",
  nboot = 200,
  sp = 1e6
)
ipwm_out
}

}
\references{
Gravel, C. A., & Platt, R. W. (2018). Weighted estimation for confounded binary outcomes subject to misclassification. \emph{Statistics in medicine}, 37(3), 425-436. https://doi.org/10.1002/sim.7522

Penning de Vries, B. B. L., van Smeden, M., & Groenwold, R. H. H. (2020). A weighting method for simultaneous adjustment for confounding and joint exposure-outcome misclassifications. \emph{Statistical Methods in Medical Research}, 0(0), 1-15. https://doi.org/10.1177/0962280220960172
}
\author{
Bas B. L. Penning de Vries, \email{b.b.l.penning_de_vries@lumc.nl}
}

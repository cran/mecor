% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{sim}
\alias{sim}
\title{Simulated dataset for the \link[mecor]{ipwm} function}
\format{
A data frame with 5000 rows and 14 variables:
\describe{
  \item{L1}{covariate, binary}
  \item{L2}{covariate, continuous}
  \item{L3}{covariate, binary}
  \item{L4}{covariate, continuous}
  \item{L5}{covariate, binary}
  \item{L6}{covariate, binary}
  \item{L7}{covariate, continuous}
  \item{L8}{covariate, binary}
  \item{L9}{covariate, binary}
  \item{L10}{covariate, continuous}
  \item{A}{exposure, binary}
  \item{Y}{outcome, binary}
  \item{B}{misclassified exposure, binary}
  \item{Z}{misclassified outcome, binary}
}
}
\usage{
sim
}
\description{
A simulated dataset containing 5000 observations of the covariates L1-L10,
the true exposure A and true outcome Y, and the misclassified exposure B and
misclassified outcome Z.
}
\examples{
data("sim", package = "mecor")
}
\keyword{datasets}

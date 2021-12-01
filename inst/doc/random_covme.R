## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)
library(mecor)

## ----load_data, eval = TRUE---------------------------------------------------
# load internal covariate validation study
data("vat", package = "mecor")
head(vat)

## ----uncorfit, eval = TRUE----------------------------------------------------
# naive estimate of the exposure-outcome association
data(vat)
lm(ir_ln ~ wc + age, data = vat)

## ----extrc, eval = TRUE-------------------------------------------------------
# Use MeasErrorRandom for measurement error correction:
mecor(ir_ln ~ MeasErrorRandom(substitute = wc, variance = 0.25) + age,
      data = vat)

## ----extrc2, eval = TRUE------------------------------------------------------
# First, construct the variance--covariance matrix of X_star and Z: 
# ( Var(X_star)   Cov(X_star, Z)
#   Cov(Z,X_star) Var(Z)       )
# To do so, we design Q, a matrix with 1000 rows (number of observations) and 2 
# columns. The first column of Q contains all 1000 observations of X_star, each 
# minus the mean of X_star. The second column of Q contains all 1000 obervations 
# of Z, each minus the mean of Z. 
Q <- scale(cbind(vat$wc, vat$age), scale = F)
# Subsequently, the variance--covariance matrix of X_star and Z is constructed:
matrix <- t(Q) %*% Q / (length(vat$age) - 1)
# Then, the variance--covariance matrix of X and Z is constructed, by using:
# Var(X) = Var(X_star) - Var(U) <--- Var(U) is the assumed tau^2
# Cov(X, Z) = Cov(X_star, Z)    <--- since U is assumed independent of Z
matrix1 <- matrix
matrix1[1, 1] <- matrix1[1, 1] - 0.25 # tau^2 = 0.25
# Rosner et al. (1992) show that the calibration model matrix can be constructed
# by taking the inverse of the variance--covariance matrix of X and Z and by
# matrix multiplying that matrix with the variance--covariance matrix of X_star
# and Z. 
model_matrix <- solve(matrix1) %*% matrix
model_matrix
matrix1 %*% solve(matrix)
# The resulting matrix is now:
# (1/lambda1        0
#  -lambda2/lambda1 1)
# Where,
# lambda1 = Cov(X,X_star|Z) / Var(X_star|Z)
# lambda2 = Cov(X,Z|X_star) / Var(Z|X_star) 
# Or, more familiar, the calibration model,
# E[X|X_star, Z] = lambda0 + lambda1 * X_star + lambda2 * Z
lambda1 <- 1 / model_matrix[1, 1]
lambda2 <- model_matrix[2,1] * - lambda1
# From standard theory, we have,
# lambda0 = mean(X) - lambda1 * mean(X_star) - lambda2 * mean(Z)
# mean(X) = mean(X_star) since we assume random measurement error
lambda0 <- mean(vat$wc) - lambda1 * mean(vat$wc) - lambda2 * mean(vat$age)
# The calibration model matrix Lambda is defined as:
# (lambda1 lambda0 lambda2
#  0       1       0
#  0       0       1)
model_matrix <- diag(3)
model_matrix[1, 1:3] <- c(lambda1, lambda0, lambda2)
model_matrix
# The calibration model matrix is standard output of mecor, and can be found
# using:
mecor_fit <- mecor(ir_ln ~ MeasErrorRandom(wc, 0.25) + age,
                   data = vat)
mecor_fit$corfit$matrix

## ----extrc3, eval = TRUE------------------------------------------------------
# Fit naive outcome model
naive_fit <- lm(ir_ln ~ wc + age, 
                data = vat)
# Save coefficients
beta_star <- naive_fit$coefficients
# To prepare the coefficients for the measurement error correction, exchange the
# intercept and the coefficient for X_star
beta_star[1:2] <- rev(beta_star[1:2]) 
# Perform the measurement error correction:
beta <- beta_star %*% solve(model_matrix)
# Reverse the order 
beta[1:2] <- rev(beta[1:2])
beta # corrected coefficients of the outcome model


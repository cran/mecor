## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)
library(mecor)

## ----load_data, eval = TRUE---------------------------------------------------
# load internal covariate validation study
data("icvs", package = "mecor")
head(icvs)

## ----uncorfit, eval = TRUE----------------------------------------------------
# naive estimate of the exposure-outcome association
lm(Y ~ X_star + Z, data = icvs)

## ----ivr, eval = TRUE---------------------------------------------------------
# analysis restricted to the internal validation set
lm(Y ~ X + Z, data = subset(icvs, !is.na(X)))

## ----rc, eval = TRUE----------------------------------------------------------
mecor(formula = Y ~ MeasError(substitute = X_star, reference = X) + Z,
      data = icvs,
      method = "standard", # defaults to "standard"
      B = 0) # defaults to 0

## ----rc_se, eval = TRUE, results = 'hide'-------------------------------------
# save corrected fit
rc_fit <- 
  mecor(formula = Y ~ MeasError(substitute = X_star, reference = X) + Z,
        data = icvs,
        method = "standard", # defaults to "standard"
        B = 999) # defaults to 0

## ----rc_se_sum, eval = TRUE---------------------------------------------------
summary(rc_fit)

## ----rc_se2, eval = TRUE------------------------------------------------------
# fieller method ci and zero variance method ci and se's for 'rc_fit'
summary(rc_fit, zerovar = TRUE, fieller = TRUE)

## ----load_data2, eval = TRUE--------------------------------------------------
# load replicates study
data("rs", package = "mecor")
head(rs)

## ----uncorfit2, eval = TRUE---------------------------------------------------
# naive estimate of the exposure-outcome association
lm(Y ~ X_star_1 + Z1 + Z2, 
   data = rs)

## ----uncorfit3, eval = TRUE---------------------------------------------------
## calculate the mean of the three replicate measures
rs$X_star_123 <- with(rs, rowMeans(cbind(X_star_1, X_star_2, X_star_3)))
# naive estimate of the exposure-outcome association version 2
lm(Y ~ X_star_123 + Z1 + Z2,
   data = rs)

## ----rc2, eval = TRUE---------------------------------------------------------
mecor(formula = Y ~ MeasError(X_star_1, replicate = cbind(X_star_2, X_star_3)) + Z1 + Z2,
      data = rs)

## ----load_data3, eval = TRUE--------------------------------------------------
# load calibration study
data("ccs", package = "mecor")
head(ccs)

## ----uncorfit4, eval = TRUE---------------------------------------------------
## uncorrected regression
lm(Y ~ X_star + Z, 
   data = ccs)

## ----uncorfit5, eval = TRUE---------------------------------------------------
## calculate mean of three replicate measures
ccs$X_star_12 <- with(ccs, rowMeans(cbind(X_star_1, X_star_2)))
## uncorrected regression version 2
lm(Y ~ X_star_12 + Z,
   data = ccs)

## ----rc3, eval = TRUE---------------------------------------------------------
mecor(formula = Y ~ MeasError(X_star, replicate = cbind(X_star_1, X_star_2)) + Z,
      data = ccs)

## ----rc4, eval = TRUE---------------------------------------------------------
mecor(formula = Y ~ MeasError(X_star_1, replicate = X_star_2) + Z,
      data = subset(ccs, !is.na(X_star_2)))

## ----load_data4, eval = TRUE--------------------------------------------------
# load internal covariate validation study
data("ecvs", package = "mecor")
head(ecvs)
data("icvs", package = "mecor")

## ----extrc, eval = TRUE-------------------------------------------------------
# Estimate the calibration model in the external validation study
calmod <- lm(X ~ X_star + Z, 
             data = ecvs)
# Use the calibration model for measurement error correction:
mecor(Y ~ MeasErrorExt(substitute = X_star, model = calmod) + Z,
      data = icvs)

## ----extrc2, eval = TRUE------------------------------------------------------
# Use coefficients for measurement error correction:
mecor(Y ~ MeasErrorExt(X_star, model = list(coef = c(0, 0.8, 2))) + Z,
      data = icvs)


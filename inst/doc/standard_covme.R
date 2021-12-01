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
lm(ir_ln ~ wc + sex + age + tbf, data = vat)

## ----ivr, eval = TRUE---------------------------------------------------------
# analysis restricted to the internal validation set
lm(ir_ln ~ vat + sex + age + tbf, data = subset(vat, !is.na(vat)))

## ----rc, eval = TRUE----------------------------------------------------------
mecor(formula = ir_ln ~ MeasError(substitute = wc, reference = vat) + sex + age + tbf,
      data = vat,
      method = "standard", # defaults to "standard"
      B = 0) # defaults to 0

## ----rc_se, eval = TRUE, results = 'hide'-------------------------------------
# save corrected fit
rc_fit <- 
  mecor(formula = ir_ln ~ MeasError(substitute = wc, reference = vat) + age + sex + tbf,
        data = vat,
        method = "standard", # defaults to "standard"
        B = 999) # defaults to 0

## ----rc_se_sum, eval = TRUE---------------------------------------------------
summary(rc_fit)

## ----rc_se2, eval = TRUE------------------------------------------------------
# fieller method ci and zero variance method ci and se's for 'rc_fit'
summary(rc_fit, zerovar = TRUE, fieller = TRUE)

## ----load_data2, eval = TRUE--------------------------------------------------
# load replicates study
data("bloodpressure", package = "mecor")
head(bloodpressure)

## ----uncorfit2, eval = TRUE---------------------------------------------------
# naive estimate of the exposure-outcome association
lm(creatinine ~ sbp30 + age, 
   data = bloodpressure)

## ----uncorfit3, eval = TRUE---------------------------------------------------
## calculate the mean of the three replicate measures
bloodpressure$sbp_123 <- with(bloodpressure, rowMeans(cbind(sbp30, sbp60, sbp120)))
# naive estimate of the exposure-outcome association version 2
lm(creatinine ~ sbp_123 + age,
   data = bloodpressure)

## ----rc2, eval = TRUE---------------------------------------------------------
mecor(formula = creatinine ~ MeasError(sbp30, replicate = cbind(sbp60, sbp120)) + age,
      data = bloodpressure)

## ----load_data3, eval = TRUE--------------------------------------------------
# load calibration study
data("sodium", package = "mecor")
head(sodium)

## ----uncorfit4, eval = TRUE---------------------------------------------------
## uncorrected regression
lm(recall ~ diet, 
   data = sodium)

## ----uncorfit5, eval = TRUE---------------------------------------------------
## calculate mean of three replicate measures
sodium$urinary_12 <- with(sodium, rowMeans(cbind(urinary1, urinary2)))
## uncorrected regression version 2
lm(urinary_12 ~ diet,
   data = sodium)

## ----rc3, eval = TRUE---------------------------------------------------------
mecor(formula = MeasError(substitute = recall, replicate = cbind(urinary1, urinary2)) ~ diet,
      data = sodium)

## ----load_data4, eval = TRUE--------------------------------------------------
# load internal covariate validation study
data("haemoglobin_ext", package = "mecor")
head(haemoglobin_ext)
data("haemoglobin", package = "mecor")

## ----extrc, eval = TRUE-------------------------------------------------------
# Estimate the calibration model in the external validation study
calmod <- lm(capillary ~ venous, 
             data = haemoglobin)
# Use the calibration model for measurement error correction:
mecor(MeasErrorExt(substitute = capillary, model = calmod) ~ supplement,
      data = haemoglobin)

## ----extrc2, eval = TRUE------------------------------------------------------
# Use coefficients for measurement error correction:
mecor(MeasErrorExt(capillary, model = list(coef = c(-7, 1.1))) ~ supplement,
      data = haemoglobin)


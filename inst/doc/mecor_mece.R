## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)
library(mecor)

## ----example1-----------------------------------------------------------------
# simulate the trial's data
X <- rep(c(0,1), 500)
Y <- 2 * X + rnorm(1000, 0, 1) # estimand: 2
# introduce measurement error
Y_star <- 1.1 * Y + rnorm(1000, 0, 1)
trial <- cbind.data.frame(X = X, Y_star = Y_star)
# simulate an external validation data set
Y <- rnorm(100, 2, 1)
Y_star <- 1.1 * Y + rnorm(500, 0, 1)
trial_ext <- cbind.data.frame(Y = Y, Y_star = Y_star)

## ----example2-----------------------------------------------------------------
# uncorrected estimate of the trial's effect:
uncor_fit <- lm(Y_star ~ X, data = trial)
uncor_fit$coefficients

## ----example3-----------------------------------------------------------------
memod_fit <- lm(Y_star ~ Y, data = trial_ext)
memod_fit$coefficients

## ----example4-----------------------------------------------------------------
cor_fit <- mecor(MeasErrorExt(substitute = Y_star, model = memod_fit) ~ X,
                 data = trial,
                 method = "standard",
                 B = 0 # for bootstrap intervals, set to e.g. 999
                 )

## ----example5-----------------------------------------------------------------
summary(cor_fit, fieller = TRUE, zerovar = TRUE)

## ----example6-----------------------------------------------------------------
sens_fit <- mecor(MeasErrorExt(substitute = Y_star, 
                               model = list(coef = c(0, 1.1))) ~ X, 
                  data = trial,
                  method = "standard"
                  )
sens_fit


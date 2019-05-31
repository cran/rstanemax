## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE-------------------------------------------------------
library(rstanemax)
library(dplyr)
library(ggplot2)
set.seed(12345)

## ---- results="hide"-----------------------------------------------------
data(exposure.response.sample)

fit.emax <- stan_emax(response ~ exposure, data = exposure.response.sample)

## ------------------------------------------------------------------------
fit.emax

## ---- fig.show='hold'----------------------------------------------------
plot(fit.emax)

## ------------------------------------------------------------------------
class(fit.emax$stanfit)

## ------------------------------------------------------------------------
response.pred <- posterior_predict(fit.emax, newdata = c(0, 100, 1000), returnType = "tibble")

response.pred %>% select(mcmcid, exposure, respHat, response)

## ------------------------------------------------------------------------
resp.pred.quantile <- posterior_predict_quantile(fit.emax, newdata = seq(0, 1000, by = 250))
resp.pred.quantile

## ------------------------------------------------------------------------
ggplot(resp.pred.quantile, aes(exposure, ci_med)) +
  geom_line() + 
  geom_ribbon(aes(ymin=ci_low, ymax=ci_high), alpha = .5) +
  geom_ribbon(aes(ymin=pi_low, ymax=pi_high), alpha = .2) +
  labs(y = "response")

## ---- results="hide"-----------------------------------------------------
data(exposure.response.sample)

fit.emax.sigmoidal <- stan_emax(response ~ exposure, data = exposure.response.sample, gamma.fix = NULL)

## ------------------------------------------------------------------------
fit.emax.sigmoidal

## ----fig.width = 6, fig.height = 4---------------------------------------

exposure_pred <- seq(min(exposure.response.sample$exposure),
                     max(exposure.response.sample$exposure),
                     length.out = 100)

pred1 <- 
  posterior_predict_quantile(fit.emax, exposure_pred) %>% 
  mutate(model = "Emax")
pred2 <- 
  posterior_predict_quantile(fit.emax.sigmoidal, exposure_pred) %>% 
  mutate(model = "Sigmoidal Emax")

pred <- bind_rows(pred1, pred2)


ggplot(pred, aes(exposure, ci_med, color = model, fill = model)) +
  geom_line() + 
  geom_ribbon(aes(ymin=ci_low, ymax=ci_high), alpha = .3) +
  geom_ribbon(aes(ymin=pi_low, ymax=pi_high), alpha = .1, color = NA) +
  geom_point(data=exposure.response.sample, aes(exposure, response), 
             color = "black", fill = NA, size=2) +
  labs(y = "response")



 


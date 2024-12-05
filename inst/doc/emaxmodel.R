## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(rstanemax)
library(dplyr)
library(ggplot2)
set.seed(12345)

## ----results="hide"-----------------------------------------------------------
data(exposure.response.sample)

fit.emax <- stan_emax(response ~ exposure,
  data = exposure.response.sample,
  # the next line is only to make the example go fast enough
  chains = 2, iter = 1000, seed = 12345
)

## -----------------------------------------------------------------------------
fit.emax

## ----plot_example, fig.show='hold'--------------------------------------------
plot(fit.emax)

## ----plot_example_log, fig.show='hold'----------------------------------------
plot(fit.emax) + scale_x_log10() + expand_limits(x = 1)

## -----------------------------------------------------------------------------
class(extract_stanfit(fit.emax))

## -----------------------------------------------------------------------------
response.pred <- posterior_predict(fit.emax, newdata = c(0, 100, 1000), returnType = "tibble")

response.pred %>% select(mcmcid, exposure, respHat, response)

## -----------------------------------------------------------------------------
resp.pred.quantile <- posterior_predict_quantile(fit.emax, newdata = seq(0, 5000, by = 100))
resp.pred.quantile

## -----------------------------------------------------------------------------
obs.formatted <- extract_obs_mod_frame(fit.emax)

## ----plot_with_pp, fig.show='hold'--------------------------------------------
ggplot(resp.pred.quantile, aes(exposure, ci_med)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = .5) +
  geom_ribbon(aes(ymin = pi_low, ymax = pi_high), alpha = .2) +
  geom_point(
    data = obs.formatted,
    aes(y = response)
  ) +
  labs(y = "response")

## -----------------------------------------------------------------------------
posterior.fit.emax <- extract_param(fit.emax)
posterior.fit.emax

## ----results="hide"-----------------------------------------------------------
data(exposure.response.sample)

fit.emax.sigmoidal <- stan_emax(response ~ exposure,
  data = exposure.response.sample,
  gamma.fix = NULL,
  # the next line is only to make the example go fast enough
  chains = 2, iter = 1000, seed = 12345
)

## -----------------------------------------------------------------------------
fit.emax.sigmoidal

## ----plot_with_gamma_fix, fig.width = 6, fig.height = 4, fig.show='hold'------
exposure_pred <- seq(min(exposure.response.sample$exposure),
  max(exposure.response.sample$exposure),
  length.out = 100
)

pred1 <-
  posterior_predict_quantile(fit.emax, exposure_pred) %>%
  mutate(model = "Emax")
pred2 <-
  posterior_predict_quantile(fit.emax.sigmoidal, exposure_pred) %>%
  mutate(model = "Sigmoidal Emax")

pred <- bind_rows(pred1, pred2)


ggplot(pred, aes(exposure, ci_med, color = model, fill = model)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = .3) +
  geom_ribbon(aes(ymin = pi_low, ymax = pi_high), alpha = .1, color = NA) +
  geom_point(
    data = exposure.response.sample, aes(exposure, response),
    color = "black", fill = NA, size = 2
  ) +
  labs(y = "response")

## ----results="hide"-----------------------------------------------------------
data(exposure.response.sample.with.cov)

test.data <-
  mutate(exposure.response.sample.with.cov,
    SEX = ifelse(cov2 == "B0", "MALE", "FEMALE")
  )

fit.cov <- stan_emax(
  formula = resp ~ conc, data = test.data,
  param.cov = list(emax = "SEX"),
  # the next line is only to make the example go fast enough
  chains = 2, iter = 1000, seed = 12345
)

## ----plot_with_cov, fig.width = 6, fig.height = 4, fig.show='hold'------------
fit.cov
plot(fit.cov)

## ----compare_emax, fig.show='hold'--------------------------------------------
fit.cov.posterior <-
  extract_param(fit.cov)

emax.posterior <-
  fit.cov.posterior %>%
  select(mcmcid, SEX, emax) %>%
  tidyr::pivot_wider(names_from = SEX, values_from = emax) %>%
  mutate(delta = FEMALE - MALE)

ggplot2::qplot(delta, data = emax.posterior, bins = 30) +
  ggplot2::labs(x = "emax[FEMALE] - emax[MALE]")

# Credible interval of delta
quantile(emax.posterior$delta, probs = c(0.025, 0.05, 0.5, 0.95, 0.975))

# Posterior probability of emax[FEMALE] < emax[MALE]
sum(emax.posterior$delta < 0) / nrow(emax.posterior)



# rstanemax 0.1.4 (in development)

## Minor changes


# rstanemax 0.1.3

## Minor changes

* Bug fix - disable `options(lifecycle_verbosity = "error")` to avoid unnecessary errors


# rstanemax 0.1.2

## Minor changes

* You can now fix Emax with `emax.fix` argument for `stan_emax()` function.


# rstanemax 0.1.1

## Breaking changes

* For posterior prediction, the column name of the newdata needs to be the same as the one in the original input data, instead of `exposure` in the previous version.

## Major changes

* `stan_emax()` can now incorporate categorical covariates in select parameters. 
  See `vignette("emaxmodel")` for detail.

# rstanemax 0.1.0

Initial CRAN release

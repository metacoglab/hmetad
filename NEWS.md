# hmetad 0.1.2

## Bug fixes
  - Resolved error in `posterior_predict()` and `posterior_epred()` for models with multilevel effects on a single confidence level
  - `cov_matrix()` now works for scalar inputs
  - `aggregate_metad()` now removes rows with `NA` values prior to aggregation
  
## New features
  - `metac2_parameters()` function streamlines setting priors for confidence criteria

## Minor improvements and fixes
  - `aggregate_metad()` and `fit_metad()` now perform more thorough checks on the number of confidence levels, `K`
  - `aggregate_metad()` has increased efficiency

---


# hmetad 0.1.1

## Bug fixes
  - Fixed bug in `linpred_draws_metad`/`linpred_rvars_metad` where `meta_c` only used first draw

## New features
  - Added `logit` option to use Stan's `multinomial_logit_lpmf`/`categorical_logit_lpmf`

## Minor improvements and fixes
  - `aggregate_metad()` now preserves column types 
  - `aggregate_metad()` and `fit_metad()` now infer `K` using the maximum confidence level (instead of the number of unique levels)
  - `aggregate_metad()` and `fit_metad()` now have more helpful errors/messages for invalid data arguments

  - Minor updates to package documentation
  
  
---


# hmetad 0.1.0

`hmetad` is now on [CRAN](https://cran.r-project.org/package=hmetad)!

# hmetad 0.0.1


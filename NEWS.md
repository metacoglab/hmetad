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
  
# hmetad 0.1.0

`hmetad` is now on [CRAN](https://cran.r-project.org/web/packages/hmetad/index.html)!

# hmetad 0.0.1


# hmetad 0.1.1
  - column types are now preserved in `aggregate_metad`
  - `aggregate_metad`/`fit_metad` now use maximum confidence level (instead of unique levels) to infer `K`
  - fixed bug in `linpred_draws_metad`/`linpred_rvars_metad` where `meta_c` only used first draw
  - added `logit` option to use Stan's `multinomial_logit_lpmf`/`categorical_logit_lpmf`
  - minor updates to package documentation
  
# hmetad 0.1.0

`hmetad` is now on [CRAN](https://cran.r-project.org/web/packages/hmetad/index.html)!

# hmetad 0.0.1


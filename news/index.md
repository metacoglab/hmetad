# Changelog

## hmetad 0.1.2

### Bug fixes

- Resolved error in
  [`posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html)
  and
  [`posterior_epred()`](https://mc-stan.org/rstantools/reference/posterior_epred.html)
  for models with multilevel effects on a single confidence level
- [`cov_matrix()`](https://metacoglab.github.io/hmetad/reference/cov_matrix.md)
  now works for scalar inputs
- [`aggregate_metad()`](https://metacoglab.github.io/hmetad/reference/aggregate_metad.md)
  now removes rows with `NA` values prior to aggregation

### New features

- [`metac2_parameters()`](https://metacoglab.github.io/hmetad/reference/metac2_parameters.md)
  function streamlines setting priors for confidence criteria

### Minor improvements and fixes

- [`aggregate_metad()`](https://metacoglab.github.io/hmetad/reference/aggregate_metad.md)
  and
  [`fit_metad()`](https://metacoglab.github.io/hmetad/reference/fit_metad.md)
  now perform more thorough checks on the number of confidence levels,
  `K`
- [`aggregate_metad()`](https://metacoglab.github.io/hmetad/reference/aggregate_metad.md)
  has increased efficiency

------------------------------------------------------------------------

## hmetad 0.1.1

CRAN release: 2026-04-20

### Bug fixes

- Fixed bug in `linpred_draws_metad`/`linpred_rvars_metad` where
  `meta_c` only used first draw

### New features

- Added `logit` option to use Stan’s
  `multinomial_logit_lpmf`/`categorical_logit_lpmf`

### Minor improvements and fixes

- [`aggregate_metad()`](https://metacoglab.github.io/hmetad/reference/aggregate_metad.md)
  now preserves column types

- [`aggregate_metad()`](https://metacoglab.github.io/hmetad/reference/aggregate_metad.md)
  and
  [`fit_metad()`](https://metacoglab.github.io/hmetad/reference/fit_metad.md)
  now infer `K` using the maximum confidence level (instead of the
  number of unique levels)

- [`aggregate_metad()`](https://metacoglab.github.io/hmetad/reference/aggregate_metad.md)
  and
  [`fit_metad()`](https://metacoglab.github.io/hmetad/reference/fit_metad.md)
  now have more helpful errors/messages for invalid data arguments

- Minor updates to package documentation

------------------------------------------------------------------------

## hmetad 0.1.0

CRAN release: 2026-03-16

`hmetad` is now on [CRAN](https://cran.r-project.org/package=hmetad)!

## hmetad 0.0.1

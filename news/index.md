# Changelog

## hmetad 0.2.0

CRAN release: 2026-08-21

### New features

- Added `allow_negative_values` argument to `fit_metad`, which models
  M-ratio on the identity (rather than the logarithmic) scale to allow
  negative values of M-ratio
- Added ability to simulate with negative M-ratios. **Note that the
  `log_M` arguments to simulation functions are renamed to reflect
  this**
- Added functions for empirical quantities (i.e., `type1_probabilities`,
  `type2_probabilities`, `joint_probabilities`, `roc1`, `roc2`, and
  `mean_confidence`), allowing for easy comparison with model estimates
- Added functions for area under type 1 and type 2 ROCs (i.e., `auroc1`,
  `auroc2`, `auroc1_draws`, and `auroc2_draws`)
- Added a `by_response` argument to `roc2_draws` and related functions,
  allowing for type 2 ROCs collapsed across type 1 responses

### Minor improvements and fixes

- Added `.stimulus`, `.response`, `.confidence`, and `.joint_response`
  arguments to all draws functions, allowing users to specify the names
  of the corresponding columns
- Confidence levels now match across the two type 1 responses in
  `roc2_draws`

## hmetad 0.1.2

CRAN release: 2026-05-15

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

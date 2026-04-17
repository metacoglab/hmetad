# Changelog

## hmetad 0.1.1

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

## hmetad 0.1.0

CRAN release: 2026-03-16

`hmetad` is now on [CRAN](https://cran.r-project.org/package=hmetad)!

## hmetad 0.0.1

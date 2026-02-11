# Given the distances between successive confidence thresholds, calculate the average of the cumulative distances to 0.

Given the distances between successive confidence thresholds, calculate
the average of the cumulative distances to 0.

## Usage

``` r
metacognitive_bias(..., rvar = FALSE)
```

## Arguments

- ...:

  a series of distances between confidence thresholds

- rvar:

  if `TRUE`, use
  [`posterior::rvar_sum`](https://mc-stan.org/posterior/reference/rvar-summaries-within-draws.html)
  in place of `sum`

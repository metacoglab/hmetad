# Obtain posterior draws of the area under the type 2 receiver operating characteristic (ROC) curve.

Given a data frame and a meta-d' model, adds estimates of AUROC2
(optionally for each type 1 response). For `auroc2_draws` and
`add_auroc2_draws`, estimates are returned in a tidy tibble with one row
per posterior draw. For `auroc2_rvars` and `add_auroc2_rvars`,
parameters are returned as
[`posterior::rvar`](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata`.

## Usage

``` r
auroc2_draws(
  object,
  newdata,
  ...,
  .response = "response",
  .confidence = "confidence",
  by_response = TRUE
)

add_auroc2_draws(newdata, object, ...)

auroc2_rvars(
  object,
  newdata,
  ...,
  .response = "response",
  .confidence = "confidence",
  by_response = TRUE
)

add_auroc2_rvars(newdata, object, ...)
```

## Arguments

- object:

  The `brms` model with the `metad` family

- newdata:

  A data frame from which to generate posterior predictions

- ...:

  Additional parameters passed to
  [tidybayes::epred_draws](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)

- .response:

  The name of "response" column

- .confidence:

  The name of "confidence" column

- by_response:

  If `TRUE` (default), compute separate ROCs for each type 1 response.
  Otherwise, average ROCs across both type 1 responses.

## Value

a tibble containing posterior draws of the pseudo type 1 ROC with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `auroc2_draws` and
  `add_auroc2_draws`, identifiers for the posterior sample

- `{.response}`: the type 1 response for perceived stimulus presence
  (\\R \in \\0, 1\\\\)

- `{.confidence}`: the type 2 confidence response (\\C \in \[1, K\]\\)

- `p_fa2`: the cumulative probability of an incorrect response (\\P(C\ge
  c \\\vert\\ R\ne S)\\)

- `p_hit2`: the cumulative probability of a correct response (\\P(C\ge c
  \\\vert\\ R = S)\\)

## See also

[`auroc2()`](https://metacoglab.github.io/hmetad/reference/auroc2.md),
[`tidybayes::epred_draws()`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html),
[`tidybayes::epred_rvars()`](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

## Examples

``` r
# \donttest{
newdata <- tidyr::tibble(.row = 1)

# compute type 2 ROC curve
# equivalent to `add_auroc2_draws(newdata, example_model())`
auroc2_draws(example_model(), newdata)
#> # A tibble: 2,000 × 4
#> # Groups:   .row, response [2]
#>     .row response .draw auroc2
#>    <int>    <int> <int>  <dbl>
#>  1     1        0     1  0.659
#>  2     1        0     2  0.640
#>  3     1        0     3  0.656
#>  4     1        0     4  0.690
#>  5     1        0     5  0.659
#>  6     1        0     6  0.658
#>  7     1        0     7  0.671
#>  8     1        0     8  0.657
#>  9     1        0     9  0.683
#> 10     1        0    10  0.651
#> # ℹ 1,990 more rows

# use posterior::rvar for additional efficiency
# equivalent to `add_auroc2_rvars(newdata, example_model())`
auroc2_rvars(example_model(), newdata)
#> # A tibble: 2 × 3
#> # Groups:   .row [1]
#>    .row response        auroc2
#>   <int>    <int>    <rvar[1d]>
#> 1     1        0  0.67 ± 0.017
#> 2     1        1  0.66 ± 0.017
# }
```

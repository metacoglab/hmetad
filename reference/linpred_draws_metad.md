# Obtain posterior draws of meta-d' model parameters

Given a data frame and a meta-d' model, adds estimates of all model
parameters. For `linpred_draws_metad` and `add_linpred_draws_metad`,
parameters are returned in a tidy tibble with one row per posterior
draw. For `linpred_rvars_metad` and `add_linpred_rvars_metad`,
parameters are returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata`.

## Usage

``` r
linpred_draws_metad(object, newdata, ..., pivot_longer = FALSE)

add_linpred_draws_metad(newdata, object, ..., pivot_longer = FALSE)

linpred_rvars_metad(object, newdata, ..., pivot_longer = FALSE)

add_linpred_rvars_metad(newdata, object, ..., pivot_longer = FALSE)
```

## Arguments

- object:

  The `brms` model with the `metad` family

- newdata:

  A data frame from which to generate posterior predictions

- ...:

  Additional arguments passed to
  [tidybayes::add_linpred_draws](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)
  or
  [tidybayes::add_linpred_rvars](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

- pivot_longer:

  Return the draws in long format?

  - if `TRUE`, resulting data frame has one row per posterior draw per
    model parameter

  - if `FALSE` (default), resulting data frame has one row per posterior
    draw

## Value

a tibble containing posterior draws of model parameters with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `linpred_draws_metad`,
  identifiers for the posterior sample

- `.variable`, `.value`: if `pivot_longer=TRUE`, `.variable` identifies
  different meta-d' model parameters and `.value` stores posterior
  samples

- `M`, `dprime`, `c`, `meta_dprime`, `meta_c`, `meta_c2_0_<k>`,
  `meta_c2_1_<k>`: if `pivot_longer=FALSE`, posterior samples of all
  meta-d' model parameters

## See also

[`tidybayes::linpred_draws()`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html),
[`tidybayes::linpred_rvars()`](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

## Examples

``` r
# \donttest{
newdata <- tidyr::tibble(.row = 1)

# obtain model parameters (wide format)
# equivalent to `add_linpred_draws_metad(newdata, example_model())`
linpred_draws_metad(example_model(), newdata)
#> # A tibble: 1,000 × 15
#> # Groups:   .row [1]
#>     .row .chain .iteration .draw     M dprime        c meta_dprime   meta_c
#>    <int>  <int>      <int> <int> <dbl>  <dbl>    <dbl>       <dbl>    <dbl>
#>  1     1     NA         NA     1 0.876  1.21   0.0608         1.06  0.0608 
#>  2     1     NA         NA     2 1.34   0.902 -0.00698        1.20 -0.00698
#>  3     1     NA         NA     3 1.29   0.841  0.0279         1.09  0.0279 
#>  4     1     NA         NA     4 1.38   0.811  0.0592         1.12  0.0592 
#>  5     1     NA         NA     5 1.03   1.03   0.00816        1.06  0.00816
#>  6     1     NA         NA     6 1.26   0.964 -0.00439        1.21 -0.00439
#>  7     1     NA         NA     7 1.05   1.08  -0.0253         1.14 -0.0253 
#>  8     1     NA         NA     8 0.994  1.07   0.00983        1.06  0.00983
#>  9     1     NA         NA     9 1.15   0.934  0.00784        1.07  0.00784
#> 10     1     NA         NA    10 1.27   0.980  0.00993        1.25  0.00993
#> # ℹ 990 more rows
#> # ℹ 6 more variables: meta_c2_0_1 <dbl>, meta_c2_0_2 <dbl>, meta_c2_0_3 <dbl>,
#> #   meta_c2_1_1 <dbl>, meta_c2_1_2 <dbl>, meta_c2_1_3 <dbl>

# obtain model parameters (long format)
# equivalent to `add_linpred_draws_metad(newdata, example_model(), pivot_longer = TRUE)`
linpred_draws_metad(example_model(), newdata, pivot_longer = TRUE)
#> # A tibble: 11,000 × 6
#> # Groups:   .row, .variable [11]
#>     .row .chain .iteration .draw .variable    .value
#>    <int>  <int>      <int> <int> <chr>         <dbl>
#>  1     1     NA         NA     1 M            0.876 
#>  2     1     NA         NA     1 dprime       1.21  
#>  3     1     NA         NA     1 c            0.0608
#>  4     1     NA         NA     1 meta_dprime  1.06  
#>  5     1     NA         NA     1 meta_c       0.0608
#>  6     1     NA         NA     1 meta_c2_0_1 -0.332 
#>  7     1     NA         NA     1 meta_c2_0_2 -0.825 
#>  8     1     NA         NA     1 meta_c2_0_3 -1.53  
#>  9     1     NA         NA     1 meta_c2_1_1  0.563 
#> 10     1     NA         NA     1 meta_c2_1_2  1.08  
#> # ℹ 10,990 more rows

# obtain model parameters (wide format, posterior::rvar)
# equivalent to `add_linpred_rvars_metad(newdata, example_model())`
linpred_rvars_metad(example_model(), newdata)
#> # A tibble: 1 × 12
#> # Groups:   .row [1]
#>    .row           M     dprime              c meta_dprime         meta_c
#>   <dbl>  <rvar[1d]> <rvar[1d]>     <rvar[1d]>  <rvar[1d]>     <rvar[1d]>
#> 1     1  1.1 ± 0.16  1 ± 0.083  0.026 ± 0.041  1.1 ± 0.13  0.026 ± 0.041
#> # ℹ 6 more variables: meta_c2_0_1 <rvar[1d]>, meta_c2_0_2 <rvar[1d]>,
#> #   meta_c2_0_3 <rvar[1d]>, meta_c2_1_1 <rvar[1d]>, meta_c2_1_2 <rvar[1d]>,
#> #   meta_c2_1_3 <rvar[1d]>

# obtain model parameters (long format, posterior::rvar)
# equivalent to `add_linpred_rvars_metad(newdata, example_model(), pivot_longer = TRUE)`
linpred_rvars_metad(example_model(), newdata, pivot_longer = TRUE)
#> # A tibble: 11 × 3
#> # Groups:   .row, .variable [11]
#>     .row .variable            .value
#>    <dbl> <chr>            <rvar[1d]>
#>  1     1 M             1.094 ± 0.159
#>  2     1 dprime        1.024 ± 0.083
#>  3     1 c             0.026 ± 0.041
#>  4     1 meta_dprime   1.112 ± 0.127
#>  5     1 meta_c        0.026 ± 0.041
#>  6     1 meta_c2_0_1  -0.453 ± 0.044
#>  7     1 meta_c2_0_2  -0.968 ± 0.051
#>  8     1 meta_c2_0_3  -1.602 ± 0.066
#>  9     1 meta_c2_1_1   0.504 ± 0.041
#> 10     1 meta_c2_1_2   1.042 ± 0.049
#> 11     1 meta_c2_1_3   1.584 ± 0.063
# }
```

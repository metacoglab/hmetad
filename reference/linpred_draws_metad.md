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
newdata <- tidyr::tibble(.row = 1)

# obtain model parameters (wide format)
# equivalent to `add_linpred_draws_metad(newdata, example_model)`
linpred_draws_metad(example_model, newdata)
#> # A tibble: 1,000 × 15
#> # Groups:   .row [1]
#>     .row .chain .iteration .draw     M dprime       c meta_dprime  meta_c
#>    <int>  <int>      <int> <int> <dbl>  <dbl>   <dbl>       <dbl>   <dbl>
#>  1     1     NA         NA     1 1.07   1.03  -0.0303        1.11 -0.0303
#>  2     1     NA         NA     2 1.19   1.06  -0.0200        1.26 -0.0200
#>  3     1     NA         NA     3 1.29   0.928 -0.0623        1.20 -0.0623
#>  4     1     NA         NA     4 0.958  1.14   0.101         1.09  0.101 
#>  5     1     NA         NA     5 1.08   1.05   0.126         1.14  0.126 
#>  6     1     NA         NA     6 1.20   0.995  0.0572        1.20  0.0572
#>  7     1     NA         NA     7 1.07   1.08  -0.0559        1.15 -0.0559
#>  8     1     NA         NA     8 1.12   0.984 -0.0592        1.10 -0.0592
#>  9     1     NA         NA     9 1.28   1.01  -0.0244        1.30 -0.0244
#> 10     1     NA         NA    10 1.21   0.900 -0.0106        1.09 -0.0106
#> # ℹ 990 more rows
#> # ℹ 6 more variables: meta_c2_0_1 <dbl>, meta_c2_0_2 <dbl>, meta_c2_0_3 <dbl>,
#> #   meta_c2_1_1 <dbl>, meta_c2_1_2 <dbl>, meta_c2_1_3 <dbl>
# \donttest{
# obtain model parameters (long format)
# equivalent to `add_linpred_draws_metad(newdata, example_model, pivot_longer = TRUE)`
linpred_draws_metad(example_model, newdata, pivot_longer = TRUE)
#> # A tibble: 11,000 × 6
#> # Groups:   .row, .variable [11]
#>     .row .chain .iteration .draw .variable    .value
#>    <int>  <int>      <int> <int> <chr>         <dbl>
#>  1     1     NA         NA     1 M            1.07  
#>  2     1     NA         NA     1 dprime       1.03  
#>  3     1     NA         NA     1 c           -0.0303
#>  4     1     NA         NA     1 meta_dprime  1.11  
#>  5     1     NA         NA     1 meta_c      -0.0303
#>  6     1     NA         NA     1 meta_c2_0_1 -0.553 
#>  7     1     NA         NA     1 meta_c2_0_2 -0.971 
#>  8     1     NA         NA     1 meta_c2_0_3 -1.41  
#>  9     1     NA         NA     1 meta_c2_1_1  0.477 
#> 10     1     NA         NA     1 meta_c2_1_2  1.03  
#> # ℹ 10,990 more rows

# obtain model parameters (wide format, posterior::rvar)
# equivalent to `add_linpred_rvars_metad(newdata, example_model)`
linpred_rvars_metad(example_model, newdata)
#> # A tibble: 1 × 12
#> # Groups:   .row [1]
#>    .row           M     dprime              c meta_dprime         meta_c
#>   <dbl>  <rvar[1d]> <rvar[1d]>     <rvar[1d]>  <rvar[1d]>     <rvar[1d]>
#> 1     1  1.1 ± 0.15  1 ± 0.082  0.015 ± 0.043  1.1 ± 0.12  0.015 ± 0.043
#> # ℹ 6 more variables: meta_c2_0_1 <rvar[1d]>, meta_c2_0_2 <rvar[1d]>,
#> #   meta_c2_0_3 <rvar[1d]>, meta_c2_1_1 <rvar[1d]>, meta_c2_1_2 <rvar[1d]>,
#> #   meta_c2_1_3 <rvar[1d]>

# obtain model parameters (long format, posterior::rvar)
# equivalent to `add_linpred_rvars_metad(newdata, example_model, pivot_longer = TRUE)`
linpred_rvars_metad(example_model, newdata, pivot_longer = TRUE)
#> # A tibble: 11 × 3
#> # Groups:   .row, .variable [11]
#>     .row .variable            .value
#>    <dbl> <chr>            <rvar[1d]>
#>  1     1 M             1.108 ± 0.150
#>  2     1 dprime        1.024 ± 0.082
#>  3     1 c             0.015 ± 0.043
#>  4     1 meta_dprime   1.127 ± 0.122
#>  5     1 meta_c        0.015 ± 0.043
#>  6     1 meta_c2_0_1  -0.491 ± 0.045
#>  7     1 meta_c2_0_2  -0.966 ± 0.050
#>  8     1 meta_c2_0_3  -1.432 ± 0.058
#>  9     1 meta_c2_1_1   0.482 ± 0.043
#> 10     1 meta_c2_1_2   1.033 ± 0.051
#> 11     1 meta_c2_1_3   1.547 ± 0.067
# }
```

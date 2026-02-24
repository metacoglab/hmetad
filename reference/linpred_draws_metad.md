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

add_linpred_rvars_metad(newdata, object, pivot_longer = FALSE)
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

## Examples

``` r
if (FALSE) { # \dontrun{
# running few iterations so example runs quickly, use more in practice
example_data <- sim_metad(N_trials = 1000)
example_model <- fit_metad(N ~ 1, example_data, chains = 1, iter = 500)
} # }
example_model <- hmetad:::example_model
newdata <- tidyr::tibble(.row = 1)

# obtain model parameters (wide format)
linpred_draws_metad(example_model, newdata)
#> # A tibble: 250 × 15
#> # Groups:   .row [1]
#>     .row .chain .iteration .draw     M dprime       c meta_dprime  meta_c
#>    <int>  <int>      <int> <int> <dbl>  <dbl>   <dbl>       <dbl>   <dbl>
#>  1     1     NA         NA     1 1.07   1.03  -0.0303        1.11 -0.0303
#>  2     1     NA         NA     2 1.19   1.06  -0.0200        1.26 -0.0303
#>  3     1     NA         NA     3 1.29   0.928 -0.0623        1.20 -0.0303
#>  4     1     NA         NA     4 0.958  1.14   0.101         1.09 -0.0303
#>  5     1     NA         NA     5 1.08   1.05   0.126         1.14 -0.0303
#>  6     1     NA         NA     6 1.20   0.995  0.0572        1.20 -0.0303
#>  7     1     NA         NA     7 1.07   1.08  -0.0559        1.15 -0.0303
#>  8     1     NA         NA     8 1.12   0.984 -0.0592        1.10 -0.0303
#>  9     1     NA         NA     9 1.28   1.01  -0.0244        1.30 -0.0303
#> 10     1     NA         NA    10 1.21   0.900 -0.0106        1.09 -0.0303
#> # ℹ 240 more rows
#> # ℹ 6 more variables: meta_c2_0_1 <dbl>, meta_c2_0_2 <dbl>, meta_c2_0_3 <dbl>,
#> #   meta_c2_1_1 <dbl>, meta_c2_1_2 <dbl>, meta_c2_1_3 <dbl>
if (FALSE) { # \dontrun{
add_linpred_draws_metad(newdata, example_model)
} # }

if (FALSE) { # \dontrun{
# obtain model parameters (long format)
linpred_draws_metad(example_model, newdata, pivot_longer = TRUE)
add_linpred_draws_metad(newdata, example_model, pivot_longer = TRUE)
} # }

if (FALSE) { # \dontrun{
# obtain model parameters (wide format, posterior::rvar)
linpred_rvars_metad(example_model, newdata)
add_linpred_rvars_metad(newdata, example_model)
} # }

if (FALSE) { # \dontrun{
# obtain model parameters (long format, posterior::rvar)
linpred_rvars_metad(example_model, newdata, pivot_longer = TRUE)
add_linpred_rvars_metad(newdata, example_model, pivot_longer = TRUE)
} # }
```

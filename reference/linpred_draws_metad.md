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
# running few iterations so example runs quickly, use more in practice
m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 1.8e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.18 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:   1 / 500 [  0%]  (Warmup)
#> Chain 1: Iteration:  50 / 500 [ 10%]  (Warmup)
#> Chain 1: Iteration: 100 / 500 [ 20%]  (Warmup)
#> Chain 1: Iteration: 150 / 500 [ 30%]  (Warmup)
#> Chain 1: Iteration: 200 / 500 [ 40%]  (Warmup)
#> Chain 1: Iteration: 250 / 500 [ 50%]  (Warmup)
#> Chain 1: Iteration: 251 / 500 [ 50%]  (Sampling)
#> Chain 1: Iteration: 300 / 500 [ 60%]  (Sampling)
#> Chain 1: Iteration: 350 / 500 [ 70%]  (Sampling)
#> Chain 1: Iteration: 400 / 500 [ 80%]  (Sampling)
#> Chain 1: Iteration: 450 / 500 [ 90%]  (Sampling)
#> Chain 1: Iteration: 500 / 500 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.032 seconds (Warm-up)
#> Chain 1:                0.021 seconds (Sampling)
#> Chain 1:                0.053 seconds (Total)
#> Chain 1: 
newdata <- tidyr::tibble(.row = 1)

# obtain model parameters (wide format)
linpred_draws_metad(m, newdata)
#> # A tibble: 250 × 15
#> # Groups:   .row [1]
#>     .row .chain .iteration .draw     M dprime      c meta_dprime meta_c
#>    <int>  <int>      <int> <int> <dbl>  <dbl>  <dbl>       <dbl>  <dbl>
#>  1     1     NA         NA     1 0.777  1.18  -0.109       0.914 -0.109
#>  2     1     NA         NA     2 0.305  1.27  -0.172       0.387 -0.109
#>  3     1     NA         NA     3 0.741  1.19  -0.230       0.883 -0.109
#>  4     1     NA         NA     4 0.337  1.25  -0.413       0.420 -0.109
#>  5     1     NA         NA     5 0.362  1.31  -0.439       0.475 -0.109
#>  6     1     NA         NA     6 0.258  1.57  -0.449       0.406 -0.109
#>  7     1     NA         NA     7 0.274  1.61  -0.250       0.440 -0.109
#>  8     1     NA         NA     8 1.09   0.721 -0.167       0.783 -0.109
#>  9     1     NA         NA     9 1.30   0.623 -0.266       0.809 -0.109
#> 10     1     NA         NA    10 0.438  1.16  -0.182       0.508 -0.109
#> # ℹ 240 more rows
#> # ℹ 6 more variables: meta_c2_0_1 <dbl>, meta_c2_0_2 <dbl>, meta_c2_0_3 <dbl>,
#> #   meta_c2_1_1 <dbl>, meta_c2_1_2 <dbl>, meta_c2_1_3 <dbl>
add_linpred_draws_metad(newdata, m)
#> # A tibble: 250 × 15
#> # Groups:   .row [1]
#>     .row .chain .iteration .draw     M dprime      c meta_dprime meta_c
#>    <int>  <int>      <int> <int> <dbl>  <dbl>  <dbl>       <dbl>  <dbl>
#>  1     1     NA         NA     1 0.777  1.18  -0.109       0.914 -0.109
#>  2     1     NA         NA     2 0.305  1.27  -0.172       0.387 -0.109
#>  3     1     NA         NA     3 0.741  1.19  -0.230       0.883 -0.109
#>  4     1     NA         NA     4 0.337  1.25  -0.413       0.420 -0.109
#>  5     1     NA         NA     5 0.362  1.31  -0.439       0.475 -0.109
#>  6     1     NA         NA     6 0.258  1.57  -0.449       0.406 -0.109
#>  7     1     NA         NA     7 0.274  1.61  -0.250       0.440 -0.109
#>  8     1     NA         NA     8 1.09   0.721 -0.167       0.783 -0.109
#>  9     1     NA         NA     9 1.30   0.623 -0.266       0.809 -0.109
#> 10     1     NA         NA    10 0.438  1.16  -0.182       0.508 -0.109
#> # ℹ 240 more rows
#> # ℹ 6 more variables: meta_c2_0_1 <dbl>, meta_c2_0_2 <dbl>, meta_c2_0_3 <dbl>,
#> #   meta_c2_1_1 <dbl>, meta_c2_1_2 <dbl>, meta_c2_1_3 <dbl>

# obtain model parameters (long format)
linpred_draws_metad(m, newdata, pivot_longer = TRUE)
#> # A tibble: 2,750 × 6
#> # Groups:   .row, .variable [11]
#>     .row .chain .iteration .draw .variable   .value
#>    <int>  <int>      <int> <int> <chr>        <dbl>
#>  1     1     NA         NA     1 M            0.777
#>  2     1     NA         NA     1 dprime       1.18 
#>  3     1     NA         NA     1 c           -0.109
#>  4     1     NA         NA     1 meta_dprime  0.914
#>  5     1     NA         NA     1 meta_c      -0.109
#>  6     1     NA         NA     1 meta_c2_0_1 -0.342
#>  7     1     NA         NA     1 meta_c2_0_2 -1.01 
#>  8     1     NA         NA     1 meta_c2_0_3 -1.54 
#>  9     1     NA         NA     1 meta_c2_1_1  0.382
#> 10     1     NA         NA     1 meta_c2_1_2  1.03 
#> # ℹ 2,740 more rows
add_linpred_draws_metad(newdata, m, pivot_longer = TRUE)
#> # A tibble: 2,750 × 6
#> # Groups:   .row, .variable [11]
#>     .row .chain .iteration .draw .variable   .value
#>    <int>  <int>      <int> <int> <chr>        <dbl>
#>  1     1     NA         NA     1 M            0.777
#>  2     1     NA         NA     1 dprime       1.18 
#>  3     1     NA         NA     1 c           -0.109
#>  4     1     NA         NA     1 meta_dprime  0.914
#>  5     1     NA         NA     1 meta_c      -0.109
#>  6     1     NA         NA     1 meta_c2_0_1 -0.342
#>  7     1     NA         NA     1 meta_c2_0_2 -1.01 
#>  8     1     NA         NA     1 meta_c2_0_3 -1.54 
#>  9     1     NA         NA     1 meta_c2_1_1  0.382
#> 10     1     NA         NA     1 meta_c2_1_2  1.03 
#> # ℹ 2,740 more rows

# obtain model parameters (wide format, posterior::rvar)
linpred_rvars_metad(m, newdata)
#> # A tibble: 1 × 12
#> # Groups:   .row [1]
#>    .row            M      dprime             c  meta_dprime        meta_c
#>   <dbl>   <rvar[1d]>  <rvar[1d]>    <rvar[1d]>   <rvar[1d]>    <rvar[1d]>
#> 1     1  0.52 ± 0.39  1.1 ± 0.24  -0.13 ± 0.13  0.52 ± 0.36  -0.13 ± 0.13
#> # ℹ 6 more variables: meta_c2_0_1 <rvar[1d]>, meta_c2_0_2 <rvar[1d]>,
#> #   meta_c2_0_3 <rvar[1d]>, meta_c2_1_1 <rvar[1d]>, meta_c2_1_2 <rvar[1d]>,
#> #   meta_c2_1_3 <rvar[1d]>
add_linpred_rvars_metad(newdata, m)
#> # A tibble: 1 × 12
#> # Groups:   .row [1]
#>    .row            M      dprime             c  meta_dprime        meta_c
#>   <dbl>   <rvar[1d]>  <rvar[1d]>    <rvar[1d]>   <rvar[1d]>    <rvar[1d]>
#> 1     1  0.52 ± 0.39  1.1 ± 0.24  -0.13 ± 0.13  0.52 ± 0.36  -0.13 ± 0.13
#> # ℹ 6 more variables: meta_c2_0_1 <rvar[1d]>, meta_c2_0_2 <rvar[1d]>,
#> #   meta_c2_0_3 <rvar[1d]>, meta_c2_1_1 <rvar[1d]>, meta_c2_1_2 <rvar[1d]>,
#> #   meta_c2_1_3 <rvar[1d]>

# obtain model parameters (long format, posterior::rvar)
linpred_rvars_metad(m, newdata, pivot_longer = TRUE)
#> # A tibble: 11 × 3
#> # Groups:   .row, .variable [11]
#>     .row .variable          .value
#>    <dbl> <chr>          <rvar[1d]>
#>  1     1 M             0.52 ± 0.39
#>  2     1 dprime        1.05 ± 0.24
#>  3     1 c            -0.13 ± 0.13
#>  4     1 meta_dprime   0.52 ± 0.36
#>  5     1 meta_c       -0.13 ± 0.13
#>  6     1 meta_c2_0_1  -0.43 ± 0.13
#>  7     1 meta_c2_0_2  -1.04 ± 0.15
#>  8     1 meta_c2_0_3  -1.61 ± 0.17
#>  9     1 meta_c2_1_1   0.32 ± 0.13
#> 10     1 meta_c2_1_2   0.79 ± 0.15
#> 11     1 meta_c2_1_3   1.40 ± 0.18
add_linpred_rvars_metad(newdata, m, pivot_longer = TRUE)
#> # A tibble: 11 × 3
#> # Groups:   .row, .variable [11]
#>     .row .variable          .value
#>    <dbl> <chr>          <rvar[1d]>
#>  1     1 M             0.52 ± 0.39
#>  2     1 dprime        1.05 ± 0.24
#>  3     1 c            -0.13 ± 0.13
#>  4     1 meta_dprime   0.52 ± 0.36
#>  5     1 meta_c       -0.13 ± 0.13
#>  6     1 meta_c2_0_1  -0.43 ± 0.13
#>  7     1 meta_c2_0_2  -1.04 ± 0.15
#>  8     1 meta_c2_0_3  -1.61 ± 0.17
#>  9     1 meta_c2_1_1   0.32 ± 0.13
#> 10     1 meta_c2_1_2   0.79 ± 0.15
#> 11     1 meta_c2_1_3   1.40 ± 0.18
```

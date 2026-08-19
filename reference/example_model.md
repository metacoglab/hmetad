# Example meta-d' model for model post-processing

A model fit to the simulated data
[example_data](https://metacoglab.github.io/hmetad/reference/example_data.md).
This model includes one constant set of parameters, with no multilevel
structure.

## Usage

``` r
example_model()
```

## Value

A `brmsfit` object fitted to simulated data

## See also

[`fit_metad()`](https://metacoglab.github.io/hmetad/reference/fit_metad.md)

## Examples

``` r
# \donttest{
# inspect summary of posterior distribution
summary(example_model())
#>  Family: metad__4__normal__absolute__multinomial 
#>   Links: mu = log 
#> Formula: N ~ 1 
#>    Data: data.aggregated (Number of observations: 1) 
#>   Draws: 4 chains, each with iter = 500; warmup = 250; thin = 1;
#>          total post-warmup draws = 1000
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     0.08      0.14    -0.17     0.34 1.01      740      760
#> 
#> Further Distributional Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> dprime              1.03      0.08     0.86     1.20 1.01      788      692
#> c                   0.03      0.04    -0.06     0.11 1.00      892      740
#> metac2zero1diff     0.48      0.04     0.41     0.55 1.01      754      701
#> metac2zero2diff     0.51      0.04     0.44     0.60 1.01     1172      872
#> metac2zero3diff     0.63      0.06     0.53     0.75 1.00      974      511
#> metac2one1diff      0.48      0.03     0.42     0.55 1.00     1191      794
#> metac2one2diff      0.54      0.04     0.46     0.62 1.00      872      607
#> metac2one3diff      0.54      0.05     0.45     0.64 1.00      882      554
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).

# obtain posterior expectations
epred_draws_metad(example_model(), tidyr::tibble(.row = 1))
#> # A tibble: 16,000 × 9
#> # Groups:   .row, stimulus, joint_response, response, confidence [16]
#>     .row stimulus joint_response response confidence .epred .chain .iteration
#>    <int>    <int>          <int>    <int>      <int>  <dbl>  <int>      <int>
#>  1     1        0              1        0          4  0.129     NA         NA
#>  2     1        0              1        0          4  0.145     NA         NA
#>  3     1        0              1        0          4  0.134     NA         NA
#>  4     1        0              1        0          4  0.140     NA         NA
#>  5     1        0              1        0          4  0.131     NA         NA
#>  6     1        0              1        0          4  0.153     NA         NA
#>  7     1        0              1        0          4  0.135     NA         NA
#>  8     1        0              1        0          4  0.132     NA         NA
#>  9     1        0              1        0          4  0.160     NA         NA
#> 10     1        0              1        0          4  0.140     NA         NA
#> # ℹ 15,990 more rows
#> # ℹ 1 more variable: .draw <int>
# }
```

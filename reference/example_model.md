# Example meta-d' model for model post-processing

A model fit to the simulated data
[example_data](https://metacoglab.github.io/hmetad/reference/example_data.md).
This model includes one constant set of parameters, with no multilevel
structure.

## Usage

``` r
example_model
```

## Format

A `brmsfit` object

## Source

Generated using the code `fit_metad(N ~ 1, example_data, iter = 500)`

## See also

[`fit_metad()`](https://metacoglab.github.io/hmetad/reference/fit_metad.md)

## Examples

``` r
# inspect summary of posterior distribution
summary(example_model)
#>  Family: metad__4__normal__absolute__multinomial 
#>   Links: mu = log 
#> Formula: N ~ 1 
#>    Data: data.aggregated (Number of observations: 1) 
#>   Draws: 4 chains, each with iter = 500; warmup = 250; thin = 1;
#>          total post-warmup draws = 1000
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     0.09      0.14    -0.20     0.34 1.00      716      734
#> 
#> Further Distributional Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> dprime              1.02      0.08     0.87     1.19 1.00      936      699
#> c                   0.01      0.04    -0.07     0.10 1.01      855      767
#> metac2zero1diff     0.51      0.04     0.44     0.58 1.01      972      767
#> metac2zero2diff     0.47      0.04     0.40     0.55 1.01     1388      852
#> metac2zero3diff     0.47      0.05     0.38     0.56 1.00     1489      904
#> metac2one1diff      0.47      0.03     0.40     0.53 1.00     1067      649
#> metac2one2diff      0.55      0.04     0.47     0.63 1.00     1112      736
#> metac2one3diff      0.51      0.05     0.43     0.61 1.01     1076      662
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).

# obtain posterior expectations
epred_draws_metad(example_model, tidyr::tibble(.row = 1))
#> # A tibble: 16,000 × 9
#> # Groups:   .row, stimulus, joint_response, response, confidence [16]
#>     .row stimulus joint_response response confidence .epred .chain .iteration
#>    <int>    <int>          <int>    <int>      <dbl>  <dbl>  <int>      <int>
#>  1     1        0              1        0          4  0.191     NA         NA
#>  2     1        0              1        0          4  0.209     NA         NA
#>  3     1        0              1        0          4  0.171     NA         NA
#>  4     1        0              1        0          4  0.174     NA         NA
#>  5     1        0              1        0          4  0.196     NA         NA
#>  6     1        0              1        0          4  0.185     NA         NA
#>  7     1        0              1        0          4  0.183     NA         NA
#>  8     1        0              1        0          4  0.176     NA         NA
#>  9     1        0              1        0          4  0.184     NA         NA
#> 10     1        0              1        0          4  0.172     NA         NA
#> # ℹ 15,990 more rows
#> # ℹ 1 more variable: .draw <int>
```

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
#> Intercept     0.08      0.15    -0.20     0.35 1.00      839      761
#> 
#> Further Distributional Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> dprime              1.02      0.08     0.85     1.20 1.00     1032      828
#> c                   0.03      0.04    -0.05     0.10 1.00      842      795
#> metac2zero1diff     0.48      0.04     0.41     0.55 1.00     1116      677
#> metac2zero2diff     0.52      0.04     0.44     0.59 1.00     1116      849
#> metac2zero3diff     0.63      0.05     0.53     0.75 1.00     1318      732
#> metac2one1diff      0.48      0.03     0.42     0.54 1.00      895      844
#> metac2one2diff      0.54      0.04     0.47     0.62 1.01      883      736
#> metac2one3diff      0.54      0.05     0.45     0.64 1.00     1207      806
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).

# obtain posterior expectations
epred_draws_metad(example_model(), tidyr::tibble(.row = 1))
#> # A tibble: 16,000 × 9
#> # Groups:   .row, stimulus, joint_response, response, confidence [16]
#>     .row stimulus joint_response response confidence .epred .chain .iteration
#>    <int>    <int>          <int>    <int>      <dbl>  <dbl>  <int>      <int>
#>  1     1        0              1        0          4  0.164     NA         NA
#>  2     1        0              1        0          4  0.126     NA         NA
#>  3     1        0              1        0          4  0.119     NA         NA
#>  4     1        0              1        0          4  0.129     NA         NA
#>  5     1        0              1        0          4  0.133     NA         NA
#>  6     1        0              1        0          4  0.153     NA         NA
#>  7     1        0              1        0          4  0.144     NA         NA
#>  8     1        0              1        0          4  0.153     NA         NA
#>  9     1        0              1        0          4  0.124     NA         NA
#> 10     1        0              1        0          4  0.165     NA         NA
#> # ℹ 15,990 more rows
#> # ℹ 1 more variable: .draw <int>
# }
```

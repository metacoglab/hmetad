# Obtain a vector of the names of the `K-1` parameters representing the differences between successive confidence criteria for the meta-d' model with `K` levels of confidence.

Obtain a vector of the names of the `K-1` parameters representing the
differences between successive confidence criteria for the meta-d' model
with `K` levels of confidence.

## Usage

``` r
metac2_parameters(K, response = "both")
```

## Arguments

- K:

  The number of confidence levels

- response:

  If "both", list confidence criteria parameters for both "0" and "1"
  responses. If "zero" or "0", list only confidence criteria for the "0"
  response. If "one" or "1", list only confidence criteria for the "1"
  response.

## Examples

``` r
# list confidence criteria parameters for K=3 confidence levels
metac2_parameters(K = 3)
#> [1] "metac2zero1diff" "metac2zero2diff" "metac2one1diff"  "metac2one2diff" 

# list parameters for "0" responses
metac2_parameters(K = 3, response = "zero")
#> [1] "metac2zero1diff" "metac2zero2diff"

# useful for setting model priors
set_prior("normal(0, 1)", class = "b", dpar = metac2_parameters(K = 4))
#>         prior class coef group resp            dpar nlpar   lb   ub tag source
#>  normal(0, 1)     b                 metac2zero1diff       <NA> <NA>       user
#>  normal(0, 1)     b                 metac2zero2diff       <NA> <NA>       user
#>  normal(0, 1)     b                 metac2zero3diff       <NA> <NA>       user
#>  normal(0, 1)     b                  metac2one1diff       <NA> <NA>       user
#>  normal(0, 1)     b                  metac2one2diff       <NA> <NA>       user
#>  normal(0, 1)     b                  metac2one3diff       <NA> <NA>       user
```

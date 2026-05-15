# Simulated data for example model fitting

A simulated data set of 1000 trials from a two-alternative forced choice
task with 4 levels of confidence.

## Usage

``` r
example_data()
```

## Value

A tibble of 1000 observations containing the following columns:

- `trial`: the trial number

- `stimulus`: the stimulus presence (`0` or `1`)

- `response`: the simulated type 1 response

- `confidence`: the simulated type 2 response

- `correct`: the accuracy of the simulated type 1 response

- `dprime`, `c`, `meta_dprime`, `M`, `meta_c2_0`, `meta_c2_1`: the
  parameters of the model used for simulation

- `theta`, `theta_1`, `theta_2`: the joint, type 1, and type 2 response
  probabilities of the model used for simulation

## See also

[`sim_metad()`](https://metacoglab.github.io/hmetad/reference/sim_metad.md)

## Examples

``` r
# Fit an empty model on the example data
# (remove empty=TRUE to actually fit the model)
fit_metad(N ~ 1, example_data(), empty = TRUE)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#>  Family: metad__4__normal__absolute__multinomial 
#>   Links: mu = log 
#> Formula: N ~ 1 
#>    Data: data.aggregated (Number of observations: 1) 
#> 
#> The model does not contain posterior draws.
```

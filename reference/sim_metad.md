# Simulate from the meta-d' model

Generate a simulated dataset from the meta-d' model with sensitivity
`dprime`, response bias `c`, metacognitive efficiency `M`, and distances
between confidence thresholds `c2_0_diff` and `c2_1_diff` (for the two
responses).

## Usage

``` r
sim_metad(
  N_trials = 100,
  dprime = 1,
  c = 0,
  M = 1,
  c2_0_diff = rep(0.5, 3),
  c2_1_diff = rep(0.5, 3),
  metac_absolute = TRUE,
  summarize = FALSE,
  lcdf = normal_lcdf,
  lccdf = normal_lccdf
)
```

## Arguments

- N_trials:

  Total number of trials to simulate. Half of these trials will have
  `stimulus=0` and half will have `stimulus=1`.

- dprime:

  The sensitivity of the signal detection agent to simulate

- c:

  The response bias of the signal detection agent to simulate

- M:

  The metacognitive efficiency of the agent, where negative values
  indicate below chance metacognitive sensitivity, `0` indicates an
  absence of metacognitive sensitivity, values between `0` and `1`
  indicate metacognitive inefficiency,
  1`indicates optimal metacognitive sensitivity, and values greater than`1\`
  indicate metacognitive hyper-efficiency.

- c2_0_diff, c2_1_diff:

  Distances between confidence thresholds for `"0"` and `"1"` responses,
  such that `meta_c2_0 = meta_c - cumsum(c2_0_diff)` and
  `meta_c2_1 = meta_c + cumsum(c2_1_diff)`.

- metac_absolute:

  Determines how to fix the type 1 threshold for modeling confidence
  ratings. If `metac_absolute=TRUE`, `meta_c = c`. Otherwise,
  `meta_c = M * c`.

- summarize:

  Aggregate the data?

  - If `summarize=FALSE`, returns a dataset with one row per
    observation.

  - If `summarize=TRUE`, returns an aggregated dataset where `n` is the
    number of observations per response, accuracy, and confidence level.

- lcdf, lccdf:

  The log (complement) cumulative distribution function of the
  underlying signal distribution. By default, uses a
  `normal(+/-dprime/2, 1)` distribution.

## Value

A simulated dataset of type 1 responses and confidence ratings, with
columns:

- `trial`: the simulated trial number

- `stimulus`: the value of the stimulus on each trial (either `0` or
  `1`)

- `response`: the simulated type 1 response (either `0` or `1`)

- `correct`: whether `stimulus==response` (either `0` or `1`)

- `confidence`: the simulated type 2 response (from `1` to
  `length(c2_0_diff)+1`)

- `dprime:theta_2`: the simulated agent's parameter values

If `summarize=TRUE`, the `trial` column is replaced with an `n` column
indicating the number of simulated type 1/type 2 responses for each
possible value.

## Examples

``` r
sim_metad(N_trials = 10)
#> # A tibble: 10 × 14
#> # Groups:   stimulus, response, confidence [8]
#>    trial stimulus response correct confidence dprime     c meta_dprime     M
#>    <int>    <int>    <int>   <int>      <int>  <dbl> <dbl>       <dbl> <dbl>
#>  1     1        0        0       1          1      1     0           1     1
#>  2     2        0        0       1          1      1     0           1     1
#>  3     3        0        0       1          2      1     0           1     1
#>  4     4        0        0       1          4      1     0           1     1
#>  5     5        0        1       0          1      1     0           1     1
#>  6     1        1        0       0          2      1     0           1     1
#>  7     2        1        0       0          4      1     0           1     1
#>  8     3        1        1       1          1      1     0           1     1
#>  9     4        1        1       1          1      1     0           1     1
#> 10     5        1        1       1          4      1     0           1     1
#> # ℹ 5 more variables: meta_c2_0 <list>, meta_c2_1 <list>, theta <dbl>,
#> #   theta_1 <dbl>, theta_2 <dbl>
sim_metad(N_trials = 10000, summarize = TRUE)
#> # A tibble: 16 × 14
#> # Groups:   stimulus [2]
#>    stimulus response correct confidence     n dprime     c meta_dprime     M
#>       <int>    <int>   <int>      <int> <int>  <dbl> <dbl>       <dbl> <dbl>
#>  1        0        0       1          1   967      1     0           1     1
#>  2        0        0       1          2   951      1     0           1     1
#>  3        0        0       1          3   727      1     0           1     1
#>  4        0        0       1          4   828      1     0           1     1
#>  5        0        1       0          1   706      1     0           1     1
#>  6        0        1       0          2   487      1     0           1     1
#>  7        0        1       0          3   213      1     0           1     1
#>  8        0        1       0          4   121      1     0           1     1
#>  9        1        0       0          1   754      1     0           1     1
#> 10        1        0       0          2   455      1     0           1     1
#> 11        1        0       0          3   241      1     0           1     1
#> 12        1        0       0          4   110      1     0           1     1
#> 13        1        1       1          1   981      1     0           1     1
#> 14        1        1       1          2   937      1     0           1     1
#> 15        1        1       1          3   766      1     0           1     1
#> 16        1        1       1          4   756      1     0           1     1
#> # ℹ 5 more variables: meta_c2_0 <list>, meta_c2_1 <list>, theta <dbl>,
#> #   theta_1 <dbl>, theta_2 <dbl>
sim_metad(N_trials = 10, c2_0_diff = 1, c2_1_diff = 1)
#> # A tibble: 10 × 14
#> # Groups:   stimulus, response, confidence [5]
#>    trial stimulus response correct confidence dprime     c meta_dprime     M
#>    <int>    <int>    <int>   <int>      <int>  <dbl> <dbl>       <dbl> <dbl>
#>  1     1        0        0       1          1      1     0           1     1
#>  2     2        0        0       1          1      1     0           1     1
#>  3     3        0        0       1          1      1     0           1     1
#>  4     4        0        0       1          1      1     0           1     1
#>  5     5        0        1       0          1      1     0           1     1
#>  6     1        1        0       0          1      1     0           1     1
#>  7     2        1        1       1          1      1     0           1     1
#>  8     3        1        1       1          1      1     0           1     1
#>  9     4        1        1       1          1      1     0           1     1
#> 10     5        1        1       1          2      1     0           1     1
#> # ℹ 5 more variables: meta_c2_0 <list>, meta_c2_1 <list>, theta <dbl>,
#> #   theta_1 <dbl>, theta_2 <dbl>
```

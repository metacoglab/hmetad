# Fitting the meta-d' model

## Introduction

This vignette demonstrates how to use the `hmetad` package to fit the
meta-d’ model ([Maniscalco & Lau, 2012](#ref-maniscalco2012)) to a
canonical metacognition experiment which requires a binary decision
together with a confidence rating on each trial.

## Data preparation

To get a better idea of what kind of datasets the `hmetad` package is
designed for, we can start by simulating one (see
[`help('sim_metad')`](https://metacoglab.github.io/hmetad/reference/sim_metad.md)
for a description of the data simulation function):

``` r

library(tidyverse)
library(tidybayes)
library(hmetad)

d <- sim_metad(
  N_trials = 1000, dprime = .75, c = -.5, M = .33,
  c2_0 = c(.25, .75, 1), c2_1 = c(.5, 1, 1.25)
)
```

    #> # A tibble: 1,000 × 4
    #> # Groups:   stimulus, response, confidence [16]
    #>    trial stimulus response confidence
    #>    <int>    <int>    <int>      <int>
    #>  1     1        0        0          1
    #>  2     2        0        0          1
    #>  3     3        0        0          1
    #>  4     4        0        0          1
    #>  5     5        0        0          1
    #>  6     6        0        0          1
    #>  7     7        0        0          1
    #>  8     8        0        0          1
    #>  9     9        0        0          1
    #> 10    10        0        0          1
    #> # ℹ 990 more rows

As you can see, our dataset has a column for the `trial` number, the
presented `stimulus` on each trial (`0` or `1`), the participant’s type
1 response (`0` or `1`), and the corresponding type 2 response
(confidence; `1:K`). The trials in this dataset are sorted by
`stimulus`, `response`, and `confidence` because this data set is
simulated, but otherwise this should look very similar to the kind of
data that you would get from running your own experiment.

### Type 1, type 2, and joint responses

One wrinkle is that some paradigms do not collect a separate decision
(i.e., type 1 response) and confidence rating (i.e., type 2
response)—rather, they collect a single rating reflecting both the
primary decision and confidence. For example, instead of a binary type 1
response and a type 2 response ranging from `1` to `K` (where `K` is the
maximum confidence level), sometimes participants are asked to make a
rating on a scale from `1` to `2*K`, where `1` represents a confidence
`"0"` response, `K` represents an uncertain `"0"` response, `K+1`
represents an uncertain `"1"` response, and `2*K` represents a confident
`"1"` response. We will refer to this as a *joint response*, as it is a
combination of the type 1 response and the type 2 response.

If you would like to convert joint response data into separate type 1
and type 2 responses, you can use the corresponding functions
`type1_response` and `type2_response`. For example, if instead we had a
dataset that looked like this:

    #> # A tibble: 1,000 × 2
    #>    trial joint_response
    #>    <int>          <int>
    #>  1     1              4
    #>  2     2              4
    #>  3     3              4
    #>  4     4              4
    #>  5     5              4
    #>  6     6              4
    #>  7     7              4
    #>  8     8              4
    #>  9     9              4
    #> 10    10              4
    #> # ℹ 990 more rows

Then we could convert our joint response like so:

``` r

d.joint_response |>
  mutate(
    response = type1_response(joint_response, K = 4),
    confidence = type2_response(joint_response, K = 4)
  )
#> # A tibble: 1,000 × 4
#>    trial joint_response response confidence
#>    <int>          <int>    <int>      <int>
#>  1     1              4        0          1
#>  2     2              4        0          1
#>  3     3              4        0          1
#>  4     4              4        0          1
#>  5     5              4        0          1
#>  6     6              4        0          1
#>  7     7              4        0          1
#>  8     8              4        0          1
#>  9     9              4        0          1
#> 10    10              4        0          1
#> # ℹ 990 more rows
```

Similarly, you can also convert the separate responses into a joint
response:

``` r

d |>
  mutate(joint_response = joint_response(response, confidence, K = 4))
#> # A tibble: 1,000 × 5
#> # Groups:   stimulus, response, confidence [16]
#>    trial stimulus response confidence joint_response
#>    <int>    <int>    <int>      <int>          <int>
#>  1     1        0        0          1              4
#>  2     2        0        0          1              4
#>  3     3        0        0          1              4
#>  4     4        0        0          1              4
#>  5     5        0        0          1              4
#>  6     6        0        0          1              4
#>  7     7        0        0          1              4
#>  8     8        0        0          1              4
#>  9     9        0        0          1              4
#> 10    10        0        0          1              4
#> # ℹ 990 more rows
```

Note that in both cases we need to specify that our confidence scale has
`K=4` levels (meaning that our joint type 1/type 2 scale has `8`
levels).

### Signed and unsigned binary numbers

Often datasets will use `-1` and `1` instead of `0` and `1` to represent
the two possible stimuli and type 1 responses. While the `hmetad`
package is designed to use the *unsigned* (`0` or `1`) version, it
provides helper functions to convert between the two:

``` r

to_unsigned(c(-1, 1))
#> [1] 0 1
```

``` r

to_signed(c(0, 1))
#> [1] -1  1
```

### Data aggregation

Finally, to ensure that the model runs efficiently, the `hmetad` package
currently requires data to be aggregated. If it is easier, the `hmetad`
package will aggregate your data for you when you fit your model. But if
you would like to do so manually (e.g., for plotting or follow-up
analyses), the `aggregate_metad` function can do this for you:

``` r

d.summary <- aggregate_metad(d)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
```

    #> # A tibble: 1 × 3
    #>     N_0   N_1 N[,"N_0_1"] [,"N_0_2"] [,"N_0_3"] [,"N_0_4"] [,"N_0_5"] [,"N_0_6"]
    #>   <int> <int>       <int>      <int>      <int>      <int>      <int>      <int>
    #> 1   500   500           2         58        118         44         82        135
    #> # ℹ 1 more variable: N[7:16] <int>

The resulting data frame has three columns: `N_0` is the number of
trials with `stimulus==0`, `N_1` is the number of trials with
`stimulus==1`, and `N` is a matrix containing the number of joint
responses for each of the two possible stimuli (with column names
indicating the `stimulus` and `joint_response`).

If you would like to use variable name other than `N` for the counts,
you can change the name with the `.name` argument:

``` r

aggregate_metad(d, .name = "y")
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 1 × 3
#>     y_0   y_1 y[,"y_0_1"] [,"y_0_2"] [,"y_0_3"] [,"y_0_4"] [,"y_0_5"] [,"y_0_6"]
#>   <int> <int>       <int>      <int>      <int>      <int>      <int>      <int>
#> 1   500   500           2         58        118         44         82        135
#> # ℹ 1 more variable: y[7:16] <int>
```

Similarly, you are able to use different column names for `stimulus`,
`response`, and `confidence` (or `stimulus` and `joint_response`):

``` r

d |>
  rename(
    s = stimulus,
    r = response,
    c = confidence
  ) |>
  aggregate_metad(.stimulus = "s", .response = "r", .confidence = "c")
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 1 × 3
#>     N_0   N_1 N[,"N_0_1"] [,"N_0_2"] [,"N_0_3"] [,"N_0_4"] [,"N_0_5"] [,"N_0_6"]
#>   <int> <int>       <int>      <int>      <int>      <int>      <int>      <int>
#> 1   500   500           2         58        118         44         82        135
#> # ℹ 1 more variable: N[7:16] <int>
```

If you have other columns in your dataset (e.g., `participant` or
`condition` columns) that you would like to be aggregated separately,
you can simply add them to the function call:

``` r

aggregate_metad(d, participant, condition)
```

Finally, note that `aggregate_metad` automatically estimates the number
of confidence levels based on the maximum value of the confidence or
joint response column in your data. This usually works fine, but may
fail in cases with missing data (e.g., no participant gives a confidence
rating of `3` on a `4`-point scale). The number of confidence levels can
be specified manually using the argument `K`:

``` r

aggregate_metad(d, participant, condition, K = 4)
```

## Model fitting

To fit the model, we can use the `fit_metad` function. This function is
simply a wrapper around
[`brms::brm`](https://paulbuerkner.com/brms/reference/brm.html), so
users are **strongly** encouraged to become familiar with [the `brms`
package](https://paulbuerkner.com/brms/) before model fitting. In
particular, users are likely to run into convergence errors using the
default (flat) priors for model parameters, so we recommend doing
careful prior predictive checks to set weakly informed priors (see
[Schad et al., 2021](#ref-schad2021toward) for more information).

Since `aggregate_metad` will place our dataset has our trial counts into
a column named `N` by default, we can use `N` as our response variable
even if our data is not yet aggregated. To fit a model with fixed values
for each parameter, then, we can use the formula `N ~ 1`:

``` r

m <- fit_metad(N ~ 1,
  data = d, init = 0,
  prior = prior(normal(0, 1), class = Intercept) +
    prior(normal(0, 1), class = dprime) +
    prior(normal(0, 1), class = c) +
    set_prior("lognormal(0, 1)", class = metac2_parameters(K = 4))
)
```

    #>  Family: metad__4__normal__absolute__multinomial 
    #>   Links: mu = log 
    #> Formula: N ~ 1 
    #>    Data: data.aggregated (Number of observations: 1) 
    #>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    #>          total post-warmup draws = 4000
    #> 
    #> Regression Coefficients:
    #>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> Intercept    -0.81      0.37    -1.62    -0.20 1.00     3631     2385
    #> 
    #> Further Distributional Parameters:
    #>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> dprime              0.69      0.08     0.53     0.86 1.00     6672     3517
    #> c                  -0.49      0.04    -0.57    -0.41 1.00     4155     3000
    #> metac2zero1diff     0.21      0.02     0.16     0.26 1.00     6720     2726
    #> metac2zero2diff     0.79      0.05     0.68     0.89 1.00     6138     3038
    #> metac2zero3diff     1.34      0.19     1.00     1.75 1.00     5573     2602
    #> metac2one1diff      0.47      0.03     0.41     0.54 1.00     4334     3111
    #> metac2one2diff      0.99      0.05     0.90     1.08 1.00     6025     3504
    #> metac2one3diff      1.29      0.11     1.09     1.50 1.00     7349     3113
    #> 
    #> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    #> and Tail_ESS are effective sample size measures, and Rhat is the potential
    #> scale reduction factor on split chains (at convergence, Rhat = 1).

Note that here we have arbitrarily chosen to use standard normal priors
for all parameters. To get a better idea of how to set informed priors,
please refer to
[`help('set_prior', package='brms')`](https://paulbuerkner.com/brms/reference/set_prior.html).

In this model, `Intercept` is our estimate of \textrm{log}(M) =
\textrm{log}\frac{\textrm{meta-}d'}{d'}, `dprime` is our estimate of d',
`c` is our estimate of c, `metac2zero1diff` and `metac2zero2diff` are
the distances between successive confidence thresholds for `"0"`
responses, and `metac2one1diff` and `metac2one2diff` are the distances
between successive confidence thresholds for `"1"` responses. For each
parameter, `brms` shows you the posterior means (`Estimate`), posterior
standard deviations (`Est. Error`), upper- and lower-95% posterior
quantiles (`l-95% CI` and `u-95% CI`), as well as some convergence
metrics (`Rhat`, `Bulk_ESS`, and `Tail_ESS`).

### Manual model fitting

Most users can use `fit_metad` as above to fit their models. But in some
cases, it might be preferable to call
[`brms::brm`](https://paulbuerkner.com/brms/reference/brm.html)
directly. In such cases, the `fit_metad` function is roughly analogous
to the following code:

``` r

# calculate number of confidence levels
K <- n_distinct(d$confidence)

m <- brm(bf(...),
  data = aggregate_metad(d, ...),
  family = metad(K = K, ...),
  stanvars = stanvars_metad(K = K, ...),
  ...
)
```

Alternatively, if the only issue is with the automatic data aggregation,
one can provide the argument `aggregate=FALSE` to `fit_metad`:

``` r

d.aggregated <- aggregate_metad(d, ...)

# modify d.aggregated as needed

m <- fit_metad(bf(...), d.aggregated, aggregate = FALSE)
```

## Extract model estimates

Once we have our fitted model, there are many estimates that we can
extract from it. Although `brms` provides its own functions for
extracting posterior estimates, the `hmetad` package is designed to
interface well with the `tidybayes` package to make it easier to work
with model posterior samples.

### Parameter estimates

First, it is often useful to extract the posterior draws of the model
parameters, which we can do with `linpred_draws_metad` (which is a
wrapper around
[`tidybayes::linpred_draws`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)):

``` r

draws.metad <- tibble(.row = 1) |>
  add_linpred_draws_metad(m)
```

    #> # A tibble: 4,000 × 15
    #> # Groups:   .row [1]
    #>     .row .chain .iteration .draw     M dprime      c meta_dprime meta_c
    #>    <int>  <int>      <int> <int> <dbl>  <dbl>  <dbl>       <dbl>  <dbl>
    #>  1     1     NA         NA     1 0.397  0.590 -0.461       0.234 -0.461
    #>  2     1     NA         NA     2 0.908  0.638 -0.431       0.579 -0.431
    #>  3     1     NA         NA     3 0.292  0.781 -0.566       0.228 -0.566
    #>  4     1     NA         NA     4 0.455  0.668 -0.413       0.304 -0.413
    #>  5     1     NA         NA     5 0.520  0.685 -0.531       0.356 -0.531
    #>  6     1     NA         NA     6 0.635  0.677 -0.537       0.430 -0.537
    #>  7     1     NA         NA     7 0.287  0.742 -0.485       0.213 -0.485
    #>  8     1     NA         NA     8 0.246  0.685 -0.596       0.168 -0.596
    #>  9     1     NA         NA     9 0.231  0.821 -0.617       0.190 -0.617
    #> 10     1     NA         NA    10 0.712  0.681 -0.467       0.485 -0.467
    #> # ℹ 3,990 more rows
    #> # ℹ 6 more variables: meta_c2_0_1 <dbl>, meta_c2_0_2 <dbl>, meta_c2_0_3 <dbl>,
    #> #   meta_c2_1_1 <dbl>, meta_c2_1_2 <dbl>, meta_c2_1_3 <dbl>

This `tibble` has a separate row for every posterior sample and a
separate column for every model parameter. This format is useful for
some purposes, but it will often be useful to pivot it so that we have a
separate row for each model parameter and posterior sample:

``` r

draws.metad <- tibble(.row = 1) |>
  add_linpred_draws_metad(m, pivot_longer = TRUE)
```

    #> # A tibble: 44,000 × 6
    #> # Groups:   .row, .variable [11]
    #>     .row .chain .iteration .draw .variable     .value
    #>    <int>  <int>      <int> <int> <chr>          <dbl>
    #>  1     1     NA         NA     1 M            0.397  
    #>  2     1     NA         NA     1 dprime       0.590  
    #>  3     1     NA         NA     1 c           -0.461  
    #>  4     1     NA         NA     1 meta_dprime  0.234  
    #>  5     1     NA         NA     1 meta_c      -0.461  
    #>  6     1     NA         NA     1 meta_c2_0_1 -0.709  
    #>  7     1     NA         NA     1 meta_c2_0_2 -1.57   
    #>  8     1     NA         NA     1 meta_c2_0_3 -2.97   
    #>  9     1     NA         NA     1 meta_c2_1_1  0.00242
    #> 10     1     NA         NA     1 meta_c2_1_2  1.03   
    #> # ℹ 43,990 more rows

Now that all of the posterior samples are stored in a single column
`.value`, it is easy to get posterior summaries using
e.g. [`tidybayes::median_qi`](https://mjskay.github.io/ggdist/reference/point_interval.html):

``` r

draws.metad |>
  median_qi()
#> # A tibble: 11 × 8
#>     .row .variable    .value  .lower  .upper .width .point .interval
#>    <int> <chr>         <dbl>   <dbl>   <dbl>  <dbl> <chr>  <chr>    
#>  1     1 c           -0.492  -0.574  -0.409    0.95 median qi       
#>  2     1 dprime       0.693   0.533   0.861    0.95 median qi       
#>  3     1 M            0.459   0.198   0.815    0.95 median qi       
#>  4     1 meta_c      -0.492  -0.574  -0.409    0.95 median qi       
#>  5     1 meta_c2_0_1 -0.703  -0.789  -0.616    0.95 median qi       
#>  6     1 meta_c2_0_2 -1.49   -1.61   -1.37     0.95 median qi       
#>  7     1 meta_c2_0_3 -2.82   -3.24   -2.49     0.95 median qi       
#>  8     1 meta_c2_1_1 -0.0210 -0.0997  0.0582   0.95 median qi       
#>  9     1 meta_c2_1_2  0.972   0.878   1.07     0.95 median qi       
#> 10     1 meta_c2_1_3  2.26    2.06    2.48     0.95 median qi       
#> 11     1 meta_dprime  0.318   0.138   0.529    0.95 median qi
```

### Posterior predictions

One way to evaluate model fit is to perform a *posterior predictive
check*: to simulate data from the model’s posterior and compare our
simulated and actual data. We can do this using the function
`predicted_draws_metad` (which is a wrapper around
[`tidybayes::predicted_draws`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)):

``` r

draws.predicted <- predicted_draws_metad(m, d.summary)
```

    #> # A tibble: 64,000 × 12
    #> # Groups:   .row, N_0, N_1, N, stimulus, joint_response, response, confidence
    #> #   [16]
    #>     .row   N_0   N_1 N[,"N_0_1"] stimulus joint_response response confidence
    #>    <int> <int> <int>       <int>    <int>          <int>    <int>      <int>
    #>  1     1   500   500           2        0              1        0          4
    #>  2     1   500   500           2        0              1        0          4
    #>  3     1   500   500           2        0              1        0          4
    #>  4     1   500   500           2        0              1        0          4
    #>  5     1   500   500           2        0              1        0          4
    #>  6     1   500   500           2        0              1        0          4
    #>  7     1   500   500           2        0              1        0          4
    #>  8     1   500   500           2        0              1        0          4
    #>  9     1   500   500           2        0              1        0          4
    #> 10     1   500   500           2        0              1        0          4
    #> # ℹ 63,990 more rows
    #> # ℹ 5 more variables: N[2:16] <int>, .prediction <int>, .chain <int>,
    #> #   .iteration <int>, .draw <int>

In this data frame, we have all of the columns from our aggregated data
`d.summary` as well as `stimulus`, `joint_response`, `response`, and
`confidence` (indicating the simulated trial type), as well as
`.prediction` (indicating the number of simulated trials per trial
type). From here, we can plot the posterior predictions (points and
error-bars) against the actual data (bars):

``` r

draws.predicted |>
  group_by(.row, stimulus, joint_response, response, confidence) |>
  median_qi(.prediction) |>
  left_join(joint_probabilities(d)) |>
  ggplot(aes(x = joint_response)) +
  geom_col(aes(y = n), fill = "grey80") +
  geom_pointrange(aes(y = .prediction, ymin = .lower, ymax = .upper)) +
  facet_wrap(~stimulus, labeller = label_both) +
  theme_classic(18)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> Joining with `by = join_by(stimulus, joint_response, response,
#> confidence)`
```

![](hmetad_files/figure-html/unnamed-chunk-11-1.png)

### Posterior expectations

Usually it will be simpler to compare response probabilities rather than
raw response counts. To do this, we can use the same workflow as above
but using `epred_draws_metad` (which is a wrapper around
[`tidybayes::epred_draws`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)):

``` r

draws.epred <- epred_draws_metad(m, newdata = tibble(.row = 1))
```

    #> # A tibble: 64,000 × 9
    #> # Groups:   .row, stimulus, joint_response, response, confidence [16]
    #>     .row stimulus joint_response response confidence  .epred .chain .iteration
    #>    <int>    <int>          <int>    <int>      <int>   <dbl>  <int>      <int>
    #>  1     1        0              1        0          4 0.00257     NA         NA
    #>  2     1        0              1        0          4 0.00513     NA         NA
    #>  3     1        0              1        0          4 0.00730     NA         NA
    #>  4     1        0              1        0          4 0.00417     NA         NA
    #>  5     1        0              1        0          4 0.00441     NA         NA
    #>  6     1        0              1        0          4 0.00480     NA         NA
    #>  7     1        0              1        0          4 0.00464     NA         NA
    #>  8     1        0              1        0          4 0.00280     NA         NA
    #>  9     1        0              1        0          4 0.00527     NA         NA
    #> 10     1        0              1        0          4 0.00742     NA         NA
    #> # ℹ 63,990 more rows
    #> # ℹ 1 more variable: .draw <int>

``` r

draws.epred |>
  group_by(.row, stimulus, joint_response, response, confidence) |>
  median_qi(.epred) |>
  left_join(joint_probabilities(d)) |>
  ggplot(aes(x = joint_response)) +
  geom_col(aes(y = p), fill = "grey80") +
  geom_pointrange(aes(y = .epred, ymin = .lower, ymax = .upper)) +
  facet_wrap(~stimulus, labeller = label_both) +
  theme_classic(18)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> Joining with `by = join_by(stimulus, joint_response, response,
#> confidence)`
```

![](hmetad_files/figure-html/epred-1.png)

### Mean confidence

One can also compute implied values of mean confidence from the meta-d’
model using `mean_confidence_draws`:

``` r

tibble(.row = 1) |>
  add_mean_confidence_draws(m) |>
  median_qi(.epred) |>
  left_join(mean_confidence(d))
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> Joining with `by = join_by(stimulus, response)`
#> # A tibble: 4 × 10
#>    .row stimulus response .epred .lower .upper .width .point .interval
#>   <int>    <int>    <int>  <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>    
#> 1     1        0        0   2.06   1.98   2.14   0.95 median qi       
#> 2     1        0        1   1.92   1.84   2.00   0.95 median qi       
#> 3     1        1        0   1.95   1.86   2.04   0.95 median qi       
#> 4     1        1        1   2.08   2.01   2.15   0.95 median qi       
#> # ℹ 1 more variable: mean_confidence <dbl>
```

Here, `.epred` refers to the model-estimated mean confidence per
stimulus and response, and `.true` is the empirical mean confidence.

In addition, we can compute mean confidence marginalizing over stimuli:

``` r

tibble(.row = 1) |>
  add_mean_confidence_draws(m, by_stimulus = FALSE) |>
  median_qi(.epred) |>
  left_join(mean_confidence(d, by_stimulus = FALSE))
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> Joining with `by = join_by(response)`
#> # A tibble: 2 × 9
#>    .row response .epred .lower .upper .width .point .interval mean_confidence
#>   <int>    <int>  <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>               <dbl>
#> 1     1        0   2.02   1.95   2.10   0.95 median qi                   2.03
#> 2     1        1   2.01   1.96   2.07   0.95 median qi                   2.01
```

over responses:

``` r

tibble(.row = 1) |>
  add_mean_confidence_draws(m, by_response = FALSE) |>
  median_qi(.epred) |>
  left_join(mean_confidence(d, by_response = FALSE))
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> Joining with `by = join_by(stimulus)`
#> # A tibble: 2 × 9
#>    .row stimulus .epred .lower .upper .width .point .interval mean_confidence
#>   <int>    <int>  <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>               <dbl>
#> 1     1        0   1.98   1.93   2.03   0.95 median qi                   2   
#> 2     1        1   2.05   2.00   2.11   0.95 median qi                   2.04
```

or both over stimuli and responses:

``` r

tibble(.row = 1) |>
  add_mean_confidence_draws(m, by_stimulus = FALSE, by_response = FALSE) |>
  median_qi(.epred) |>
  left_join(mean_confidence(d, by_stimulus = FALSE, by_response = FALSE) |>
    mutate(.row = 1))
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> Joining with `by = join_by(.row)`
#> # A tibble: 1 × 8
#>    .row .epred .lower .upper .width .point .interval mean_confidence
#>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>               <dbl>
#> 1     1   2.02   1.97   2.06   0.95 median qi                   2.02
```

### Metacognitive bias

While mean confidence is often empirically informative, it is not
recommended as a measure of metacognitive bias because it is known to be
confounded by type 1 response characteristics (i.e., d' and c) and by
metacognitive sensitivity (i.e., \textrm{meta-}d', [Sherman et al.,
2018](#ref-sherman2018)). Instead, we recommend a new measure of
metacognitive bias, \textrm{meta-}\Delta, which is the distance between
the average of the confidence criteria and \textrm{meta-}c.

\textrm{meta-}\Delta can be interpreted as lying between two extremes:
when \textrm{meta-}\Delta = 0, the observer only uses the highest
confidence rating, and when \textrm{meta-}\Delta = \infty, the observer
only uses the lowest confidence rating.

To obtain estimates of \textrm{meta-}\Delta, one can use the function
`metacognitive_bias_draws`:

``` r

tibble(.row = 1) |>
  add_metacognitive_bias_draws(m) |>
  median_qi()
#> # A tibble: 2 × 8
#>    .row response metacognitive_bias .lower .upper .width .point .interval
#>   <int>    <int>              <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>    
#> 1     1        0               1.18   1.04   1.33   0.95 median qi       
#> 2     1        1               1.56   1.46   1.67   0.95 median qi
```

### Pseudo Type 1 ROC

To obtain type 1 performance as a pseudo-type 1 ROC, we can use
`add_roc1_draws`:

``` r

draws.roc1 <- tibble(.row = 1) |>
  add_roc1_draws(m)
```

    #> # A tibble: 28,000 × 9
    #> # Groups:   .row, joint_response, response, confidence [7]
    #>     .row joint_response response confidence .chain .iteration .draw  p_fa p_hit
    #>    <int>          <int>    <int>      <int>  <int>      <int> <int> <dbl> <dbl>
    #>  1     1              1        0          4     NA         NA     1 0.997 0.999
    #>  2     1              1        0          4     NA         NA     2 0.995 0.999
    #>  3     1              1        0          4     NA         NA     3 0.993 0.998
    #>  4     1              1        0          4     NA         NA     4 0.996 0.999
    #>  5     1              1        0          4     NA         NA     5 0.996 0.999
    #>  6     1              1        0          4     NA         NA     6 0.995 0.999
    #>  7     1              1        0          4     NA         NA     7 0.995 0.999
    #>  8     1              1        0          4     NA         NA     8 0.997 0.999
    #>  9     1              1        0          4     NA         NA     9 0.995 0.999
    #> 10     1              1        0          4     NA         NA    10 0.993 0.999
    #> # ℹ 27,990 more rows

Again, we have a tidy tibble with columns `.chain`, `.iteration`, and
`.draw` identifying individual posterior samples, `joint_response`,
`response`, and `confidence` identifying the different points on the
ROC, and `.row` identifying different ROCs (since our data frame has
only one row, here there is only one ROC). In addition, we also have
`p_hit` and `p_fa`, which contain posterior estimates of type 1 hit rate
(i.e., the probability of a `"1"` response with `confidence >= c` given
`stimulus==1`) and type 1 false alarm rate (i.e., the probability of a
`"1"` response with `confidence >= c` given `stimulus==0`).

For visualization, we can get posterior summaries of the ROC using
[`tidybayes::median_qi`](https://mjskay.github.io/ggdist/reference/point_interval.html)
and then simply plot as a line:

``` r

draws.roc1 |>
  median_qi(p_fa, p_hit) |>
  ggplot(aes(
    x = p_fa, xmin = p_fa.lower, xmax = p_fa.upper,
    y = p_hit, ymin = p_hit.lower, ymax = p_hit.upper
  )) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_errorbar(orientation = "y", width = .01) +
  geom_errorbar(orientation = "x", width = .01) +
  geom_line() +
  geom_point(aes(x = p_fa, y = p_hit), data = roc1(d), inherit.aes = FALSE) +
  coord_fixed(xlim = 0:1, ylim = 0:1, expand = FALSE) +
  xlab("P(False Alarm)") +
  ylab("P(Hit)") +
  theme_bw(18)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
```

![](hmetad_files/figure-html/roc1-1.png)

### Type 2 ROC

Finally, to plot type 2 performance as a type 2 ROC, we can use
`add_roc2_draws`:

``` r

draws.roc2 <- tibble(.row = 1) |>
  add_roc2_draws(m)
```

    #> # A tibble: 24,000 × 8
    #> # Groups:   .row, response, confidence [6]
    #>     .row response confidence .chain .iteration .draw  p_hit2   p_fa2
    #>    <int>    <int>      <int>  <int>      <int> <int>   <dbl>   <dbl>
    #>  1     1        0          3     NA         NA     1 0.00592 0.00359
    #>  2     1        0          3     NA         NA     2 0.0113  0.00340
    #>  3     1        0          3     NA         NA     3 0.0170  0.0113 
    #>  4     1        0          3     NA         NA     4 0.00890 0.00476
    #>  5     1        0          3     NA         NA     5 0.0104  0.00512
    #>  6     1        0          3     NA         NA     6 0.0114  0.00488
    #>  7     1        0          3     NA         NA     7 0.0102  0.00672
    #>  8     1        0          3     NA         NA     8 0.00700 0.00498
    #>  9     1        0          3     NA         NA     9 0.0126  0.00888
    #> 10     1        0          3     NA         NA    10 0.0165  0.00657
    #> # ℹ 23,990 more rows

This tibble looks the same as for `roc1_draws`, except now there are
columns for `p_hit2` representing the type 2 hit rate (i.e., the
probability of a correct response with `confidence >= c` given
`response`) and the type 2 false alarm rate (i.e., the probability of an
incorrect response with `confidence >= c` given `response`). Note that
this is the response-specific type 2 ROC, so there are two separate
curves for the two type 1 responses.

We can also plot the type 2 ROC similarly:

``` r

draws.roc2 |>
  median_qi(p_hit2, p_fa2) |>
  mutate(response = factor(response)) |>
  ggplot(aes(
    x = p_fa2, xmin = p_fa2.lower, xmax = p_fa2.upper,
    y = p_hit2, ymin = p_hit2.lower, ymax = p_hit2.upper,
    color = response
  )) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_errorbar(orientation = "y", width = .01) +
  geom_errorbar(orientation = "x", width = .01) +
  geom_line() +
  geom_point(aes(x = p_fa2, y = p_hit2, color = response),
    data = mutate(roc2(d), response = factor(response)), inherit.aes = FALSE
  ) +
  coord_fixed(xlim = 0:1, ylim = 0:1, expand = FALSE) +
  xlab("P(Type 2 False Alarm)") +
  ylab("P(Type 2 Hit)") +
  theme_bw(18)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
```

![](hmetad_files/figure-html/unnamed-chunk-15-1.png)

## References

Maniscalco, B., & Lau, H. (2012). A signal detection theoretic approach
for estimating metacognitive sensitivity from confidence ratings.
*Consciousness and Cognition*, *21*(1), 422–430.

Schad, D. J., Betancourt, M., & Vasishth, S. (2021). Toward a principled
bayesian workflow in cognitive science. *Psychological Methods*,
*26*(1), 103.

Sherman, M. T., Seth, A. K., & Barrett, A. B. (2018). Quantifying
metacognitive thresholds using signal-detection theory. *BioRxiv*,
361543.

# Estimating trial-level effects

## Introduction

By default, the `hmetad` package uses aggregated data (i.e., counts of
the number of trials with the same stimulus, type 1 response, and type 2
response.) This is because data aggregation makes model fitting and
simulation much more efficient. But sometimes researchers will be
interested in trial-level effects.

One common example would be what is often called “crossed random
effects”. For example, in a design where all participants make responses
to the same set of items, a researcher might want to estimate both
participant-level and item-level effects on their model parameters.

We can simulate data from such a design like so:

``` r
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.2.0     ✔ readr     2.2.0
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
#> ✔ purrr     1.2.1     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(tidybayes)
library(hmetad)
#> Loading required package: brms
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> 
#> The following objects are masked from 'package:tidybayes':
#> 
#>     dstudent_t, pstudent_t, qstudent_t, rstudent_t
#> 
#> The following object is masked from 'package:stats':
#> 
#>     ar

## average model parameters
K <- 3   ## number of confidence levels
mu_log_M <- -0.5
mu_dprime <- 1.5
mu_c <- 0
mu_c2_0 <- rep(-1, K-1)
mu_c2_1 <- rep(-1, K-1)

## participant-level standard deviations
sd_log_M_participant <- 0.25
sd_dprime_participant <- 0.5
sd_c_participant <- 0.33
sd_c2_0_participant <- cov_matrix(rep(0.25, K-1), diag(K-1))
sd_c2_1_participant <- cov_matrix(rep(0.25, K-1), diag(K-1))

## item-level standard deviations
sd_log_M_item <- 0.1
sd_dprime_item <- 0.5
sd_c_item <- 0.75
sd_c2_0_item <- cov_matrix(rep(0.1, K-1), diag(K-1))
sd_c2_1_item <- cov_matrix(rep(0.1, K-1), diag(K-1))



## simulate data
d <- expand_grid(participant=1:50,
            item=1:10) |>
  ## simulate participant-level differences
  group_by(participant) |>
  mutate(z_log_M_participant=rnorm(1, sd=sd_log_M_participant),
         z_dprime_participant=rnorm(1, sd=sd_dprime_participant),
         z_c_participant=rnorm(1, sd=sd_c_participant),
         z_c2_0_participant=list(rmulti_normal(1, mu=rep(0,K-1), Sigma=sd_c2_0_participant)),
         z_c2_1_participant=list(rmulti_normal(1, mu=rep(0,K-1), Sigma=sd_c2_1_participant)))|>
  ## simulate item-level differences
  group_by(item) |>
  mutate(z_log_M_item=rnorm(1, sd=sd_log_M_item),
         z_dprime_item=rnorm(1, sd=sd_dprime_item),
         z_c_item=rnorm(1, sd=sd_c_item),
         z_c2_0_item=list(rmulti_normal(1, mu=rep(0,K-1), Sigma=sd_c2_0_item)),
         z_c2_1_item=list(rmulti_normal(1, mu=rep(0,K-1), Sigma=sd_c2_1_item))) |>
  ungroup() |>
  ## compute model parameters
  mutate(log_M = mu_log_M + z_log_M_participant + z_log_M_item,
         dprime = mu_dprime + z_dprime_participant + z_dprime_item,
         c = mu_c + z_c_participant + z_c_item,
         c2_0_diff = map2(z_c2_0_participant, z_c2_0_item, 
                          ~ exp(mu_c2_0 + .x + .y)),
         c2_1_diff = map2(z_c2_1_participant, z_c2_1_item,
                          ~ exp(mu_c2_1 + .x + .y))) |>
  ## simulate two trials per participant/item (stimulus = 0 and stimulus = 1)
  mutate(trial=pmap(list(dprime, c, log_M, c2_0_diff, c2_1_diff), sim_metad, N_trials=2)) |>
  select(participant, item, trial) |> 
  unnest(trial)
d
#> # A tibble: 1,000 × 16
#>    participant  item trial stimulus response correct confidence dprime        c
#>          <int> <int> <int>    <int>    <int>   <int>      <int>  <dbl>    <dbl>
#>  1           1     1     1        0        0       1          3   1.76  0.00633
#>  2           1     1     1        1        0       0          3   1.76  0.00633
#>  3           1     2     1        0        0       1          1   2.46 -0.652  
#>  4           1     2     1        1        1       1          3   2.46 -0.652  
#>  5           1     3     1        0        0       1          2   2.34 -0.848  
#>  6           1     3     1        1        1       1          3   2.34 -0.848  
#>  7           1     4     1        0        0       1          2   2.83 -0.859  
#>  8           1     4     1        1        1       1          3   2.83 -0.859  
#>  9           1     5     1        0        0       1          3   1.88 -0.382  
#> 10           1     5     1        1        1       1          3   1.88 -0.382  
#> # ℹ 990 more rows
#> # ℹ 7 more variables: meta_dprime <dbl>, M <dbl>, meta_c2_0 <list>,
#> #   meta_c2_1 <list>, theta <dbl>, theta_1 <dbl>, theta_2 <dbl>
```

Don’t worry about the details of the simulation code- what matters is
that we have a data set with repeated measures for participants:

``` r
count(d, participant)
#> # A tibble: 50 × 2
#>    participant     n
#>          <int> <int>
#>  1           1    20
#>  2           2    20
#>  3           3    20
#>  4           4    20
#>  5           5    20
#>  6           6    20
#>  7           7    20
#>  8           8    20
#>  9           9    20
#> 10          10    20
#> # ℹ 40 more rows
```

And repeated measures for items:

``` r
count(d, item)
#> # A tibble: 10 × 2
#>     item     n
#>    <int> <int>
#>  1     1   100
#>  2     2   100
#>  3     3   100
#>  4     4   100
#>  5     5   100
#>  6     6   100
#>  7     7   100
#>  8     8   100
#>  9     9   100
#> 10    10   100
```

## Standard model with data aggregation

If we would like, we can use the `fit_metad` function on this data with
participant-level and item-level effects. However, if we aggregate the
data ourselves, we can see that the aggregation doesn’t really help us
here:

``` r
aggregate_metad(d, participant, item)
#> # A tibble: 500 × 5
#>    participant item    N_0   N_1 N[,"N_0_1"] [,"N_0_2"] [,"N_0_3"] [,"N_0_4"]
#>    <fct>       <fct> <int> <int>       <int>      <int>      <int>      <int>
#>  1 1           1         1     1           1          0          0          0
#>  2 1           2         1     1           0          0          1          0
#>  3 1           3         1     1           0          1          0          0
#>  4 1           4         1     1           0          1          0          0
#>  5 1           5         1     1           1          0          0          0
#>  6 1           6         1     1           1          0          0          0
#>  7 1           7         1     1           0          1          0          0
#>  8 1           8         1     1           0          0          0          0
#>  9 1           9         1     1           1          0          0          0
#> 10 1           10        1     1           1          0          0          0
#> # ℹ 490 more rows
#> # ℹ 1 more variable: N[5:12] <int>
```

As you can see, the aggregated data set has `500` rows (with two
observations per row), which is not much smaller than the trial-level
data that we started with! So, in this case, it will probably be easier
*not* to aggregate our data. Nevertheless, there is nothing stopping us
from fitting the model like normal:

``` r
m.multinomial <- fit_metad(
  bf(
    N ~ 1 + (1 | participant) + (1 | item),
    dprime + c +
      metac2zero1diff + metac2zero2diff +
      metac2one1diff + metac2one2diff ~
      1 + (1 | participant) + (1 | item)
  ),
  data = d, init = 0, cores=4,
  file = "models/multinomial.rds"
)
#> Compiling Stan program...
#> Trying to compile a simple C file
#> Running /opt/R/4.5.2/lib/R/bin/R CMD SHLIB foo.c
#> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
#> gcc -std=gnu2x -I"/opt/R/4.5.2/lib/R/include" -DNDEBUG   -I"/home/runner/work/_temp/Library/Rcpp/include/"  -I"/home/runner/work/_temp/Library/RcppEigen/include/"  -I"/home/runner/work/_temp/Library/RcppEigen/include/unsupported"  -I"/home/runner/work/_temp/Library/BH/include" -I"/home/runner/work/_temp/Library/StanHeaders/include/src/"  -I"/home/runner/work/_temp/Library/StanHeaders/include/"  -I"/home/runner/work/_temp/Library/RcppParallel/include/"  -I"/home/runner/work/_temp/Library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -D_HAS_AUTO_PTR_ETC=0  -include '/home/runner/work/_temp/Library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/usr/local/include    -fpic  -g -O2  -c foo.c -o foo.o
#> In file included from /home/runner/work/_temp/Library/RcppEigen/include/Eigen/Core:19,
#>                  from /home/runner/work/_temp/Library/RcppEigen/include/Eigen/Dense:1,
#>                  from /home/runner/work/_temp/Library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22,
#>                  from <command-line>:
#> /home/runner/work/_temp/Library/RcppEigen/include/Eigen/src/Core/util/Macros.h:679:10: fatal error: cmath: No such file or directory
#>   679 | #include <cmath>
#>       |          ^~~~~~~
#> compilation terminated.
#> make: *** [/opt/R/4.5.2/lib/R/etc/Makeconf:202: foo.o] Error 1
#> Start sampling
#> Warning: There were 20 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems
m.multinomial
#> Warning: There were 20 divergent transitions after warmup. Increasing
#> adapt_delta above 0.8 may help. See
#> http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#>  Family: metad__3__normal__absolute__multinomial 
#>   Links: mu = log; dprime = identity; c = identity; metac2zero1diff = log; metac2zero2diff = log; metac2one1diff = log; metac2one2diff = log 
#> Formula: N ~ 1 + (1 | participant) + (1 | item) 
#>          dprime ~ 1 + (1 | participant) + (1 | item)
#>          c ~ 1 + (1 | participant) + (1 | item)
#>          metac2zero1diff ~ 1 + (1 | participant) + (1 | item)
#>          metac2zero2diff ~ 1 + (1 | participant) + (1 | item)
#>          metac2one1diff ~ 1 + (1 | participant) + (1 | item)
#>          metac2one2diff ~ 1 + (1 | participant) + (1 | item)
#>    Data: data.aggregated (Number of observations: 500) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Multilevel Hyperparameters:
#> ~item (Number of levels: 10) 
#>                               Estimate Est.Error l-95% CI u-95% CI Rhat
#> sd(Intercept)                     0.21      0.18     0.01     0.66 1.00
#> sd(dprime_Intercept)              0.68      0.23     0.37     1.26 1.00
#> sd(c_Intercept)                   0.77      0.23     0.45     1.34 1.01
#> sd(metac2zero1diff_Intercept)     0.20      0.15     0.01     0.56 1.00
#> sd(metac2zero2diff_Intercept)     0.28      0.16     0.03     0.65 1.00
#> sd(metac2one1diff_Intercept)      0.15      0.12     0.00     0.44 1.00
#> sd(metac2one2diff_Intercept)      0.32      0.18     0.03     0.74 1.00
#>                               Bulk_ESS Tail_ESS
#> sd(Intercept)                     1149     1831
#> sd(dprime_Intercept)              1395     1773
#> sd(c_Intercept)                    907     1626
#> sd(metac2zero1diff_Intercept)     1193     1452
#> sd(metac2zero2diff_Intercept)     1009     1049
#> sd(metac2one1diff_Intercept)      1568     1531
#> sd(metac2one2diff_Intercept)      1094     1347
#> 
#> ~participant (Number of levels: 50) 
#>                               Estimate Est.Error l-95% CI u-95% CI Rhat
#> sd(Intercept)                     0.34      0.18     0.02     0.71 1.01
#> sd(dprime_Intercept)              0.56      0.14     0.28     0.84 1.00
#> sd(c_Intercept)                   0.37      0.06     0.25     0.50 1.00
#> sd(metac2zero1diff_Intercept)     0.27      0.15     0.01     0.57 1.00
#> sd(metac2zero2diff_Intercept)     0.30      0.15     0.03     0.59 1.00
#> sd(metac2one1diff_Intercept)      0.28      0.16     0.01     0.62 1.01
#> sd(metac2one2diff_Intercept)      0.51      0.14     0.24     0.79 1.00
#>                               Bulk_ESS Tail_ESS
#> sd(Intercept)                      609      548
#> sd(dprime_Intercept)              1434     1291
#> sd(c_Intercept)                   1410     2288
#> sd(metac2zero1diff_Intercept)      653      789
#> sd(metac2zero2diff_Intercept)      746      947
#> sd(metac2one1diff_Intercept)       473      666
#> sd(metac2one2diff_Intercept)      1199     1308
#> 
#> Regression Coefficients:
#>                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> Intercept                    -0.37      0.19    -0.81    -0.05 1.00     1582
#> dprime_Intercept              1.77      0.27     1.25     2.33 1.00      984
#> c_Intercept                  -0.09      0.26    -0.61     0.45 1.00      550
#> metac2zero1diff_Intercept    -1.05      0.14    -1.34    -0.79 1.00     2270
#> metac2zero2diff_Intercept    -0.76      0.14    -1.06    -0.48 1.00     1633
#> metac2one1diff_Intercept     -1.03      0.13    -1.30    -0.79 1.00     2016
#> metac2one2diff_Intercept     -1.08      0.18    -1.46    -0.75 1.00     1720
#>                           Tail_ESS
#> Intercept                     1283
#> dprime_Intercept              1229
#> c_Intercept                    946
#> metac2zero1diff_Intercept     2065
#> metac2zero2diff_Intercept     2148
#> metac2one1diff_Intercept      2260
#> metac2one2diff_Intercept      1576
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

## Data preparation

Fitting the trial-level model does not require data aggregation, however
it still requires a small amount of data preparation. To fit the model,
we will need to things: \* a column with the stimulus per trial (`0` or
`1`), and \* a column containing the joint type 1/type 2 responses per
trial (between `1` and `2*K`).

Our data already has a `stimulus` column but separate columns for the
two responses. So, we can add in a joint response column now:

``` r
d <- d |>
  mutate(joint_response = joint_response(response, confidence, K)) |>
  relocate(joint_response, .after="stimulus")
d
#> # A tibble: 1,000 × 17
#>    participant  item trial stimulus joint_response response correct confidence
#>          <int> <int> <int>    <int>          <dbl>    <int>   <int>      <int>
#>  1           1     1     1        0              1        0       1          3
#>  2           1     1     1        1              1        0       0          3
#>  3           1     2     1        0              3        0       1          1
#>  4           1     2     1        1              6        1       1          3
#>  5           1     3     1        0              2        0       1          2
#>  6           1     3     1        1              6        1       1          3
#>  7           1     4     1        0              2        0       1          2
#>  8           1     4     1        1              6        1       1          3
#>  9           1     5     1        0              1        0       1          3
#> 10           1     5     1        1              6        1       1          3
#> # ℹ 990 more rows
#> # ℹ 9 more variables: dprime <dbl>, c <dbl>, meta_dprime <dbl>, M <dbl>,
#> #   meta_c2_0 <list>, meta_c2_1 <list>, theta <dbl>, theta_1 <dbl>,
#> #   theta_2 <dbl>
```

## Model fitting

Now that we have our data, we can fit the trial-level model using
`joint_response` as our response variable, `stimulus` as an extra
variable passed to `brms`, and the argument `categorical=TRUE` to tell
`fit_metad` not to aggregate the data:

``` r
m.categorical <- fit_metad(
  bf(
    joint_response | vint(stimulus) ~ 1 + (1 | participant) + (1 | item),
    dprime + c +
      metac2zero1diff + metac2zero2diff +
      metac2one1diff + metac2one2diff ~
      1 + (1 | participant) + (1 | item)
  ),
  data = d, categorical=TRUE, init = 0,
  file = "models/categorical.rds"
)
#> Compiling Stan program...
#> Trying to compile a simple C file
#> Running /opt/R/4.5.2/lib/R/bin/R CMD SHLIB foo.c
#> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
#> gcc -std=gnu2x -I"/opt/R/4.5.2/lib/R/include" -DNDEBUG   -I"/home/runner/work/_temp/Library/Rcpp/include/"  -I"/home/runner/work/_temp/Library/RcppEigen/include/"  -I"/home/runner/work/_temp/Library/RcppEigen/include/unsupported"  -I"/home/runner/work/_temp/Library/BH/include" -I"/home/runner/work/_temp/Library/StanHeaders/include/src/"  -I"/home/runner/work/_temp/Library/StanHeaders/include/"  -I"/home/runner/work/_temp/Library/RcppParallel/include/"  -I"/home/runner/work/_temp/Library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -D_HAS_AUTO_PTR_ETC=0  -include '/home/runner/work/_temp/Library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/usr/local/include    -fpic  -g -O2  -c foo.c -o foo.o
#> In file included from /home/runner/work/_temp/Library/RcppEigen/include/Eigen/Core:19,
#>                  from /home/runner/work/_temp/Library/RcppEigen/include/Eigen/Dense:1,
#>                  from /home/runner/work/_temp/Library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22,
#>                  from <command-line>:
#> /home/runner/work/_temp/Library/RcppEigen/include/Eigen/src/Core/util/Macros.h:679:10: fatal error: cmath: No such file or directory
#>   679 | #include <cmath>
#>       |          ^~~~~~~
#> compilation terminated.
#> make: *** [/opt/R/4.5.2/lib/R/etc/Makeconf:202: foo.o] Error 1
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 0.004374 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 43.74 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 150.937 seconds (Warm-up)
#> Chain 1:                165.651 seconds (Sampling)
#> Chain 1:                316.588 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 0.002747 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 27.47 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 153.39 seconds (Warm-up)
#> Chain 2:                179.069 seconds (Sampling)
#> Chain 2:                332.459 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 0.002789 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 27.89 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 168.365 seconds (Warm-up)
#> Chain 3:                90.374 seconds (Sampling)
#> Chain 3:                258.739 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 0.002797 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 27.97 seconds.
#> Chain 4: Adjust your expectations accordingly!
#> Chain 4: 
#> Chain 4: 
#> Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 4: 
#> Chain 4:  Elapsed Time: 162.95 seconds (Warm-up)
#> Chain 4:                112.312 seconds (Sampling)
#> Chain 4:                275.262 seconds (Total)
#> Chain 4:
#> Warning: There were 32 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems
m.categorical
#> Warning: There were 32 divergent transitions after warmup. Increasing
#> adapt_delta above 0.8 may help. See
#> http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#>  Family: metad__3__normal__absolute__categorical 
#>   Links: mu = log; dprime = identity; c = identity; metac2zero1diff = log; metac2zero2diff = log; metac2one1diff = log; metac2one2diff = log 
#> Formula: joint_response | vint(stimulus) ~ 1 + (1 | participant) + (1 | item) 
#>          dprime ~ 1 + (1 | participant) + (1 | item)
#>          c ~ 1 + (1 | participant) + (1 | item)
#>          metac2zero1diff ~ 1 + (1 | participant) + (1 | item)
#>          metac2zero2diff ~ 1 + (1 | participant) + (1 | item)
#>          metac2one1diff ~ 1 + (1 | participant) + (1 | item)
#>          metac2one2diff ~ 1 + (1 | participant) + (1 | item)
#>    Data: data.aggregated (Number of observations: 1000) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Multilevel Hyperparameters:
#> ~item (Number of levels: 10) 
#>                               Estimate Est.Error l-95% CI u-95% CI Rhat
#> sd(Intercept)                     0.21      0.18     0.01     0.66 1.00
#> sd(dprime_Intercept)              0.67      0.22     0.35     1.20 1.00
#> sd(c_Intercept)                   0.75      0.23     0.44     1.29 1.00
#> sd(metac2zero1diff_Intercept)     0.20      0.15     0.01     0.54 1.00
#> sd(metac2zero2diff_Intercept)     0.26      0.16     0.02     0.62 1.00
#> sd(metac2one1diff_Intercept)      0.15      0.12     0.01     0.45 1.00
#> sd(metac2one2diff_Intercept)      0.31      0.18     0.03     0.73 1.00
#>                               Bulk_ESS Tail_ESS
#> sd(Intercept)                     1309     2135
#> sd(dprime_Intercept)              1693     2819
#> sd(c_Intercept)                   1015     1924
#> sd(metac2zero1diff_Intercept)     1484     1575
#> sd(metac2zero2diff_Intercept)     1024     1253
#> sd(metac2one1diff_Intercept)      2005     2133
#> sd(metac2one2diff_Intercept)       964     1226
#> 
#> ~participant (Number of levels: 50) 
#>                               Estimate Est.Error l-95% CI u-95% CI Rhat
#> sd(Intercept)                     0.35      0.18     0.03     0.72 1.00
#> sd(dprime_Intercept)              0.55      0.14     0.26     0.83 1.00
#> sd(c_Intercept)                   0.37      0.07     0.25     0.50 1.00
#> sd(metac2zero1diff_Intercept)     0.26      0.15     0.02     0.57 1.00
#> sd(metac2zero2diff_Intercept)     0.29      0.15     0.02     0.58 1.00
#> sd(metac2one1diff_Intercept)      0.29      0.16     0.02     0.61 1.01
#> sd(metac2one2diff_Intercept)      0.51      0.14     0.24     0.81 1.00
#>                               Bulk_ESS Tail_ESS
#> sd(Intercept)                      739     1071
#> sd(dprime_Intercept)              1098     1161
#> sd(c_Intercept)                   1545     2245
#> sd(metac2zero1diff_Intercept)     1149     1794
#> sd(metac2zero2diff_Intercept)      937     1360
#> sd(metac2one1diff_Intercept)       926     1486
#> sd(metac2one2diff_Intercept)      1347     1640
#> 
#> Regression Coefficients:
#>                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> Intercept                    -0.37      0.18    -0.78    -0.05 1.00     2820
#> dprime_Intercept              1.77      0.27     1.24     2.33 1.00     1291
#> c_Intercept                  -0.08      0.26    -0.60     0.40 1.00      845
#> metac2zero1diff_Intercept    -1.05      0.15    -1.35    -0.78 1.00     2573
#> metac2zero2diff_Intercept    -0.75      0.15    -1.06    -0.48 1.00     2468
#> metac2one1diff_Intercept     -1.03      0.13    -1.31    -0.79 1.00     2926
#> metac2one2diff_Intercept     -1.10      0.18    -1.47    -0.77 1.00     2395
#>                           Tail_ESS
#> Intercept                     2375
#> dprime_Intercept              1725
#> c_Intercept                    873
#> metac2zero1diff_Intercept     2149
#> metac2zero2diff_Intercept     2418
#> metac2one1diff_Intercept      2500
#> metac2one2diff_Intercept      2138
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

As you can see, aside from the way that the data is formatted, this
model is exactly the same as the multinomial model above.

## Extracting model estimates

Obtaining posterior estimates over model parameters, predictions, and
other estimates is very similar to the multinomial model (for details,
see
[`vignette("hmetad")`](https://metacoglab.github.io/hmetad/articles/hmetad.md)).
So, here we will focus on the type 1 ROC curves, this time using
`roc1_rvars` instead of `roc1_draws` for increased efficiency.

To get the posterior estimates, we need to specify a data set to make
predictions for, as well as a random effects formula to use in model
predictions. For example, to estimate a ROC averaging over participants
and items, we can use an empty data set with `re_formula=NA`:

``` r
roc1_rvars(m.multinomial, tibble(.row=1), re_formula=NA)
#> # A tibble: 5 × 6
#> # Groups:   .row, joint_response, response, confidence [5]
#>    .row joint_response response confidence           p_fa         p_hit
#>   <int>          <int>    <int>      <dbl>     <rvar[1d]>    <rvar[1d]>
#> 1     1              1        0          3  0.567 ± 0.107  0.95 ± 0.034
#> 2     1              2        0          2  0.366 ± 0.102  0.89 ± 0.056
#> 3     1              3        0          1  0.222 ± 0.085  0.82 ± 0.075
#> 4     1              4        1          1  0.143 ± 0.066  0.69 ± 0.096
#> 5     1              5        1          2  0.086 ± 0.047  0.55 ± 0.107
```

The process is exactly the same for the categorical model:

``` r
roc1_rvars(m.categorical, tibble(.row=1), re_formula=NA)
#> # A tibble: 5 × 6
#> # Groups:   .row, joint_response, response, confidence [5]
#>    .row joint_response response confidence           p_fa         p_hit
#>   <int>          <int>    <int>      <dbl>     <rvar[1d]>    <rvar[1d]>
#> 1     1              1        0          3  0.568 ± 0.105  0.95 ± 0.033
#> 2     1              2        0          2  0.366 ± 0.100  0.89 ± 0.055
#> 3     1              3        0          1  0.221 ± 0.083  0.82 ± 0.073
#> 4     1              4        1          1  0.143 ± 0.065  0.69 ± 0.093
#> 5     1              5        1          2  0.087 ± 0.048  0.55 ± 0.104
```

Next, to get participant-level ROCs (averaging over items), we can use a
data set with one row per participant and only the participant-level
random effects:

``` r
roc1_rvars(m.categorical, distinct(d, participant), re_formula=~ (1 | participant))
#> # A tibble: 250 × 7
#> # Groups:   .row, participant, joint_response, response, confidence [250]
#>     .row participant joint_response response confidence         p_fa
#>    <int>       <int>          <int>    <int>      <dbl>   <rvar[1d]>
#>  1     1           1              1        0          3  0.50 ± 0.14
#>  2     2           2              1        0          3  0.58 ± 0.14
#>  3     3           3              1        0          3  0.78 ± 0.12
#>  4     4           4              1        0          3  0.74 ± 0.12
#>  5     5           5              1        0          3  0.76 ± 0.12
#>  6     6           6              1        0          3  0.49 ± 0.15
#>  7     7           7              1        0          3  0.81 ± 0.11
#>  8     8           8              1        0          3  0.43 ± 0.15
#>  9     9           9              1        0          3  0.55 ± 0.15
#> 10    10          10              1        0          3  0.60 ± 0.14
#> # ℹ 240 more rows
#> # ℹ 1 more variable: p_hit <rvar[1d]>
```

We can use a similar process to get item-level ROCs (averaging over
participants):

``` r
roc1_rvars(m.categorical, distinct(d, item), re_formula=~ (1 | item))
#> # A tibble: 50 × 7
#> # Groups:   .row, item, joint_response, response, confidence [50]
#>     .row  item joint_response response confidence          p_fa          p_hit
#>    <int> <int>          <int>    <int>      <dbl>    <rvar[1d]>     <rvar[1d]>
#>  1     1     1              1        0          3  0.44 ± 0.068  0.86 ± 0.0408
#>  2     2     2              1        0          3  0.63 ± 0.067  0.99 ± 0.0055
#>  3     3     3              1        0          3  0.82 ± 0.049  0.99 ± 0.0076
#>  4     4     4              1        0          3  0.75 ± 0.062  1.00 ± 0.0023
#>  5     5     5              1        0          3  0.66 ± 0.063  0.98 ± 0.0108
#>  6     6     6              1        0          3  0.21 ± 0.057  0.77 ± 0.0557
#>  7     7     7              1        0          3  0.67 ± 0.064  0.83 ± 0.0467
#>  8     8     8              1        0          3  0.77 ± 0.059  0.99 ± 0.0073
#>  9     9     9              1        0          3  0.55 ± 0.071  0.96 ± 0.0191
#> 10    10    10              1        0          3  0.20 ± 0.056  0.68 ± 0.0636
#> # ℹ 40 more rows
```

## Other benefits

Aside from representing the data in a more convenient format, the
trial-level model should be more useful for things like model comparison
using the `loo` package, multivariate models, and mediation models.
These features should mostly work out the box but they are still under
active development, so stay tuned!

## References

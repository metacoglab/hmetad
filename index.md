# hmetad

The `hmetad` package is designed to fit the meta-d’ model for confidence
ratings ([Maniscalco & Lau, 2012](#ref-maniscalco2012),
[2014](#ref-maniscalco2014)). The `hmetad` package uses a Bayesian
modeling approach, building on and superseding previous development of
the [Hmeta-d toolbox](https://github.com/metacoglab/HMeta-d) ([Fleming,
2017](#ref-fleming2017)). A key advance is implementation as a custom
family in the [brms](https://paulbuerkner.com/brms/) package, which
itself provides a friendly interface to the probabilistic programming
language [Stan](https://mc-stan.org).

This provides major benefits:

- Model designs can be specified as simple `R` formulas
- Support for complex model designs (e.g., multilevel models,
  distributional models, multivariate models)
- Interfaces to other packages surrounding `brms` (e.g., `tidybayes`,
  `ggdist`, `bayesplot`, `loo`, `posterior`, `bridgesampling`,
  `bayestestR`)
- Computation of model-implied quantities (e.g., mean confidence, type 1
  and type 2 receiver operating characteristic curves)
- Derivation of novel metacognitive bias metrics, and well as
  established metrics of metacognitive efficiency
- Increased sampling efficiency and better convergence diagnostics

## Installation

`hmetad` is available via CRAN and can be installed using:

``` r
install.packages("hmetad")
```

Alternatively, you can install the development version of `hmetad` from
[GitHub](https://github.com/metacoglab/hmetad) with:

``` r
# install.packages("pak")
pak::pak("metacoglab/hmetad")
```

## Quick setup

Let’s say you have some data from a binary decision task with ordinal
confidence ratings:

``` R
#> # A tibble: 1,000 × 5
#>    trial stimulus response correct confidence
#>    <int>    <int>    <int>   <int>      <int>
#>  1     1        1        1       1          1
#>  2     2        0        0       1          2
#>  3     3        0        0       1          1
#>  4     4        1        1       1          3
#>  5     5        0        0       1          1
#>  6     6        0        0       1          2
#>  7     7        1        1       1          4
#>  8     8        1        1       1          1
#>  9     9        0        1       0          2
#> 10    10        1        0       0          3
#> # ℹ 990 more rows
```

You can fit an intercepts-only meta-d’ model using `fit_metad`:

``` r
library(hmetad)

m <- fit_metad(N ~ 1, data = d)
```

``` R
#>  Family: metad__4__normal__absolute__multinomial 
#>   Links: mu = log 
#> Formula: N ~ 1 
#>    Data: data.aggregated (Number of observations: 1) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     0.00      0.13    -0.26     0.24 1.00     2703     3193
#> 
#> Further Distributional Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> dprime              1.21      0.08     1.04     1.38 1.00     3510     3138
#> c                  -0.02      0.04    -0.10     0.06 1.00     3762     3386
#> metac2zero1diff     0.52      0.04     0.45     0.60 1.00     4650     3551
#> metac2zero2diff     0.52      0.04     0.44     0.60 1.00     5344     3280
#> metac2zero3diff     0.52      0.05     0.42     0.61 1.00     5160     2767
#> metac2one1diff      0.53      0.04     0.46     0.61 1.00     4170     2721
#> metac2one2diff      0.62      0.05     0.54     0.71 1.00     4464     3223
#> metac2one3diff      0.47      0.05     0.39     0.57 1.00     4961     2950
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

Now let’s say you have a more complicated design, such as a
within-participant manipulation:

``` R
#> # A tibble: 5,000 × 7
#> # Groups:   participant, condition [50]
#>    participant condition trial stimulus response correct confidence
#>          <int>     <int> <int>    <int>    <int>   <int>      <int>
#>  1           1         1     1        1        1       1          3
#>  2           1         1     2        1        0       0          3
#>  3           1         1     3        0        0       1          2
#>  4           1         1     4        0        0       1          3
#>  5           1         1     5        0        0       1          2
#>  6           1         1     6        1        1       1          4
#>  7           1         1     7        1        0       0          1
#>  8           1         1     8        1        1       1          3
#>  9           1         1     9        0        0       1          3
#> 10           1         1    10        1        0       0          4
#> # ℹ 4,990 more rows
```

To account for the repeated measures in this design, you can simply
adjust the formula to include participant-level effects:

``` r
m <- fit_metad(
  bf(
    N ~ condition + (condition | participant),
    dprime + c +
      metac2zero1diff + metac2zero2diff + metac2zero3diff +
      metac2one1diff + metac2one2diff + metac2one3diff ~
      condition + (condition | participant)
  ),
  data = d, init = "0",
  prior = prior(normal(0, 1)) +
    prior(normal(0, 1), dpar = dprime) +
    prior(normal(0, 1), dpar = c) +
    prior(normal(0, 1), dpar = metac2zero1diff) +
    prior(normal(0, 1), dpar = metac2zero2diff) +
    prior(normal(0, 1), dpar = metac2zero3diff) +
    prior(normal(0, 1), dpar = metac2one1diff) +
    prior(normal(0, 1), dpar = metac2one2diff) +
    prior(normal(0, 1), dpar = metac2one3diff)
)
```

``` R
#>  Family: metad__4__normal__absolute__multinomial 
#>   Links: mu = log; dprime = identity; c = identity; metac2zero1diff = log; metac2zero2diff = log; metac2zero3diff = log; metac2one1diff = log; metac2one2diff = log; metac2one3diff = log 
#> Formula: N ~ condition + (condition | participant) 
#>          dprime ~ condition + (condition | participant)
#>          c ~ condition + (condition | participant)
#>          metac2zero1diff ~ condition + (condition | participant)
#>          metac2zero2diff ~ condition + (condition | participant)
#>          metac2zero3diff ~ condition + (condition | participant)
#>          metac2one1diff ~ condition + (condition | participant)
#>          metac2one2diff ~ condition + (condition | participant)
#>          metac2one3diff ~ condition + (condition | participant)
#>    Data: data.aggregated (Number of observations: 50) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Multilevel Hyperparameters:
#> ~participant (Number of levels: 25) 
#>                                                           Estimate Est.Error
#> sd(Intercept)                                                 0.71      0.18
#> sd(condition2)                                                0.53      0.21
#> sd(dprime_Intercept)                                          0.49      0.10
#> sd(dprime_condition2)                                         0.56      0.12
#> sd(c_Intercept)                                               0.53      0.09
#> sd(c_condition2)                                              0.69      0.11
#> sd(metac2zero1diff_Intercept)                                 0.09      0.06
#> sd(metac2zero1diff_condition2)                                0.14      0.10
#> sd(metac2zero2diff_Intercept)                                 0.08      0.06
#> sd(metac2zero2diff_condition2)                                0.17      0.11
#> sd(metac2zero3diff_Intercept)                                 0.13      0.09
#> sd(metac2zero3diff_condition2)                                0.12      0.10
#> sd(metac2one1diff_Intercept)                                  0.07      0.05
#> sd(metac2one1diff_condition2)                                 0.17      0.10
#> sd(metac2one2diff_Intercept)                                  0.12      0.07
#> sd(metac2one2diff_condition2)                                 0.10      0.07
#> sd(metac2one3diff_Intercept)                                  0.18      0.09
#> sd(metac2one3diff_condition2)                                 0.16      0.11
#> cor(Intercept,condition2)                                    -0.56      0.29
#> cor(dprime_Intercept,dprime_condition2)                      -0.24      0.25
#> cor(c_Intercept,c_condition2)                                -0.69      0.12
#> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)    -0.20      0.57
#> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)    -0.01      0.56
#> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)    -0.32      0.56
#> cor(metac2one1diff_Intercept,metac2one1diff_condition2)      -0.38      0.55
#> cor(metac2one2diff_Intercept,metac2one2diff_condition2)      -0.15      0.57
#> cor(metac2one3diff_Intercept,metac2one3diff_condition2)      -0.22      0.55
#>                                                           l-95% CI u-95% CI
#> sd(Intercept)                                                 0.42     1.13
#> sd(condition2)                                                0.15     1.00
#> sd(dprime_Intercept)                                          0.33     0.71
#> sd(dprime_condition2)                                         0.36     0.84
#> sd(c_Intercept)                                               0.39     0.73
#> sd(c_condition2)                                              0.52     0.96
#> sd(metac2zero1diff_Intercept)                                 0.00     0.23
#> sd(metac2zero1diff_condition2)                                0.01     0.37
#> sd(metac2zero2diff_Intercept)                                 0.00     0.22
#> sd(metac2zero2diff_condition2)                                0.01     0.40
#> sd(metac2zero3diff_Intercept)                                 0.01     0.33
#> sd(metac2zero3diff_condition2)                                0.00     0.37
#> sd(metac2one1diff_Intercept)                                  0.00     0.20
#> sd(metac2one1diff_condition2)                                 0.01     0.40
#> sd(metac2one2diff_Intercept)                                  0.01     0.28
#> sd(metac2one2diff_condition2)                                 0.00     0.28
#> sd(metac2one3diff_Intercept)                                  0.02     0.37
#> sd(metac2one3diff_condition2)                                 0.01     0.41
#> cor(Intercept,condition2)                                    -0.95     0.13
#> cor(dprime_Intercept,dprime_condition2)                      -0.66     0.29
#> cor(c_Intercept,c_condition2)                                -0.87    -0.41
#> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)    -0.98     0.92
#> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)    -0.95     0.94
#> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)    -0.99     0.89
#> cor(metac2one1diff_Intercept,metac2one1diff_condition2)      -0.99     0.88
#> cor(metac2one2diff_Intercept,metac2one2diff_condition2)      -0.96     0.93
#> cor(metac2one3diff_Intercept,metac2one3diff_condition2)      -0.96     0.90
#>                                                           Rhat Bulk_ESS
#> sd(Intercept)                                             1.00     2518
#> sd(condition2)                                            1.00     1705
#> sd(dprime_Intercept)                                      1.00     2126
#> sd(dprime_condition2)                                     1.00     1675
#> sd(c_Intercept)                                           1.00     1152
#> sd(c_condition2)                                          1.00     1261
#> sd(metac2zero1diff_Intercept)                             1.00     2251
#> sd(metac2zero1diff_condition2)                            1.00     1555
#> sd(metac2zero2diff_Intercept)                             1.00     2127
#> sd(metac2zero2diff_condition2)                            1.01     1267
#> sd(metac2zero3diff_Intercept)                             1.00     1389
#> sd(metac2zero3diff_condition2)                            1.00     1890
#> sd(metac2one1diff_Intercept)                              1.00     2365
#> sd(metac2one1diff_condition2)                             1.00     1302
#> sd(metac2one2diff_Intercept)                              1.00     1635
#> sd(metac2one2diff_condition2)                             1.00     2461
#> sd(metac2one3diff_Intercept)                              1.00     1348
#> sd(metac2one3diff_condition2)                             1.00     1737
#> cor(Intercept,condition2)                                 1.00     3459
#> cor(dprime_Intercept,dprime_condition2)                   1.00     1955
#> cor(c_Intercept,c_condition2)                             1.00     1441
#> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2) 1.00     2698
#> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2) 1.00     2133
#> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2) 1.00     3589
#> cor(metac2one1diff_Intercept,metac2one1diff_condition2)   1.00     1850
#> cor(metac2one2diff_Intercept,metac2one2diff_condition2)   1.00     4226
#> cor(metac2one3diff_Intercept,metac2one3diff_condition2)   1.00     3275
#>                                                           Tail_ESS
#> sd(Intercept)                                                 2972
#> sd(condition2)                                                1879
#> sd(dprime_Intercept)                                          3094
#> sd(dprime_condition2)                                         2859
#> sd(c_Intercept)                                               1580
#> sd(c_condition2)                                              1873
#> sd(metac2zero1diff_Intercept)                                 2295
#> sd(metac2zero1diff_condition2)                                2219
#> sd(metac2zero2diff_Intercept)                                 2297
#> sd(metac2zero2diff_condition2)                                2246
#> sd(metac2zero3diff_Intercept)                                 2295
#> sd(metac2zero3diff_condition2)                                2322
#> sd(metac2one1diff_Intercept)                                  2658
#> sd(metac2one1diff_condition2)                                 2188
#> sd(metac2one2diff_Intercept)                                  2084
#> sd(metac2one2diff_condition2)                                 2832
#> sd(metac2one3diff_Intercept)                                  1405
#> sd(metac2one3diff_condition2)                                 2724
#> cor(Intercept,condition2)                                     2964
#> cor(dprime_Intercept,dprime_condition2)                       2567
#> cor(c_Intercept,c_condition2)                                 2196
#> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)     2561
#> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)     2242
#> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)     3277
#> cor(metac2one1diff_Intercept,metac2one1diff_condition2)       2351
#> cor(metac2one2diff_Intercept,metac2one2diff_condition2)       3089
#> cor(metac2one3diff_Intercept,metac2one3diff_condition2)       2764
#> 
#> Regression Coefficients:
#>                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> Intercept                     -0.08      0.19    -0.47     0.27 1.00     2577
#> dprime_Intercept               1.02      0.11     0.80     1.25 1.00     2549
#> c_Intercept                   -0.09      0.11    -0.31     0.14 1.00      793
#> metac2zero1diff_Intercept     -1.11      0.06    -1.24    -0.98 1.00     7391
#> metac2zero2diff_Intercept     -1.03      0.06    -1.16    -0.91 1.00     7749
#> metac2zero3diff_Intercept     -0.99      0.08    -1.15    -0.85 1.00     6928
#> metac2one1diff_Intercept      -0.93      0.06    -1.05    -0.82 1.00     7271
#> metac2one2diff_Intercept      -1.03      0.07    -1.17    -0.91 1.00     6686
#> metac2one3diff_Intercept      -0.97      0.08    -1.13    -0.83 1.00     5856
#> condition2                    -0.16      0.20    -0.55     0.22 1.00     3908
#> dprime_condition2              0.06      0.14    -0.21     0.33 1.00     2847
#> c_condition2                  -0.01      0.14    -0.29     0.28 1.00      992
#> metac2zero1diff_condition2    -0.03      0.09    -0.20     0.15 1.00     6781
#> metac2zero2diff_condition2    -0.01      0.09    -0.19     0.17 1.00     6671
#> metac2zero3diff_condition2    -0.03      0.10    -0.23     0.18 1.00     7187
#> metac2one1diff_condition2     -0.07      0.09    -0.24     0.10 1.00     6346
#> metac2one2diff_condition2      0.06      0.08    -0.10     0.23 1.00     7978
#> metac2one3diff_condition2     -0.06      0.10    -0.25     0.13 1.00     7365
#>                            Tail_ESS
#> Intercept                      2851
#> dprime_Intercept               2943
#> c_Intercept                    1358
#> metac2zero1diff_Intercept      3147
#> metac2zero2diff_Intercept      2871
#> metac2zero3diff_Intercept      3129
#> metac2one1diff_Intercept       2829
#> metac2one2diff_Intercept       3239
#> metac2one3diff_Intercept       3312
#> condition2                     3076
#> dprime_condition2              3001
#> c_condition2                   1660
#> metac2zero1diff_condition2     3182
#> metac2zero2diff_condition2     2993
#> metac2zero3diff_condition2     2772
#> metac2one1diff_condition2      2805
#> metac2one2diff_condition2      2915
#> metac2one3diff_condition2      2979
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

## References

Fleming, S. M. (2017). HMeta-d: Hierarchical bayesian estimation of
metacognitive efficiency from confidence ratings. *Neuroscience of
Consciousness*, *2017*(1), nix007.

Maniscalco, B., & Lau, H. (2012). A signal detection theoretic approach
for estimating metacognitive sensitivity from confidence ratings.
*Consciousness and Cognition*, *21*(1), 422–430.

Maniscalco, B., & Lau, H. (2014). Signal detection theory analysis of
type 1 and type 2 data: Meta-d′, response-specific meta-d′, and the
unequal variance SDT model. In *The cognitive neuroscience of
metacognition* (pp. 25–66). Springer.

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
#>  1     1        1        1       1          2
#>  2     2        1        0       0          2
#>  3     3        0        0       1          4
#>  4     4        1        1       1          4
#>  5     5        0        1       0          2
#>  6     6        0        1       0          3
#>  7     7        0        1       0          3
#>  8     8        1        0       0          1
#>  9     9        0        0       1          4
#> 10    10        1        1       1          3
#> # ℹ 990 more rows
```

You can fit an intercepts-only meta-d’ model using `fit_metad`:

``` r
library(hmetad)

m <- fit_metad(N ~ 1,
  data = d,
  prior = prior(normal(0, 1), class = Intercept) +
    set_prior(
      "normal(0, 1)",
      class = c(
        "dprime", "c",
        "metac2zero1diff", "metac2zero2diff", "metac2zero3diff",
        "metac2one1diff", "metac2one2diff", "metac2one3diff"
      )
    )
)
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
#> Intercept    -0.12      0.18    -0.49     0.20 1.00     4173     3401
#> 
#> Further Distributional Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> dprime              0.90      0.08     0.75     1.07 1.00     4516     3211
#> c                  -0.03      0.04    -0.11     0.05 1.00     4506     3418
#> metac2zero1diff     0.52      0.04     0.45     0.59 1.00     4850     2597
#> metac2zero2diff     0.47      0.04     0.40     0.55 1.00     7060     3012
#> metac2zero3diff     0.42      0.04     0.34     0.51 1.00     6879     3125
#> metac2one1diff      0.46      0.03     0.40     0.53 1.00     5497     3277
#> metac2one2diff      0.49      0.04     0.42     0.57 1.00     5815     3331
#> metac2one3diff      0.50      0.05     0.42     0.60 1.00     6431     2882
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
#>  2           1         1     2        0        0       1          4
#>  3           1         1     3        1        1       1          4
#>  4           1         1     4        0        0       1          4
#>  5           1         1     5        1        1       1          3
#>  6           1         1     6        1        1       1          4
#>  7           1         1     7        0        0       1          3
#>  8           1         1     8        0        1       0          2
#>  9           1         1     9        0        0       1          4
#> 10           1         1    10        0        0       1          4
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
    set_prior("normal(0, 1)",
      dpar = c(
        "dprime", "c",
        "metac2zero1diff", "metac2zero2diff", "metac2zero3diff",
        "metac2one1diff", "metac2one2diff", "metac2one3diff"
      )
    )
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
#>                                                          Estimate Est.Error
#> sd(Intercept)                                                0.77      0.28
#> sd(condition)                                                0.37      0.20
#> sd(dprime_Intercept)                                         1.09      0.20
#> sd(dprime_condition)                                         0.71      0.13
#> sd(c_Intercept)                                              1.06      0.16
#> sd(c_condition)                                              0.63      0.10
#> sd(metac2zero1diff_Intercept)                                0.07      0.06
#> sd(metac2zero1diff_condition)                                0.04      0.04
#> sd(metac2zero2diff_Intercept)                                0.16      0.12
#> sd(metac2zero2diff_condition)                                0.08      0.07
#> sd(metac2zero3diff_Intercept)                                0.10      0.08
#> sd(metac2zero3diff_condition)                                0.07      0.06
#> sd(metac2one1diff_Intercept)                                 0.11      0.10
#> sd(metac2one1diff_condition)                                 0.07      0.06
#> sd(metac2one2diff_Intercept)                                 0.12      0.12
#> sd(metac2one2diff_condition)                                 0.07      0.07
#> sd(metac2one3diff_Intercept)                                 0.12      0.10
#> sd(metac2one3diff_condition)                                 0.07      0.06
#> cor(Intercept,condition)                                    -0.79      0.32
#> cor(dprime_Intercept,dprime_condition)                      -0.91      0.04
#> cor(c_Intercept,c_condition)                                -0.94      0.03
#> cor(metac2zero1diff_Intercept,metac2zero1diff_condition)    -0.27      0.59
#> cor(metac2zero2diff_Intercept,metac2zero2diff_condition)    -0.30      0.58
#> cor(metac2zero3diff_Intercept,metac2zero3diff_condition)    -0.31      0.57
#> cor(metac2one1diff_Intercept,metac2one1diff_condition)      -0.34      0.57
#> cor(metac2one2diff_Intercept,metac2one2diff_condition)      -0.36      0.58
#> cor(metac2one3diff_Intercept,metac2one3diff_condition)      -0.33      0.58
#>                                                          l-95% CI u-95% CI Rhat
#> sd(Intercept)                                                0.33     1.39 1.00
#> sd(condition)                                                0.03     0.79 1.00
#> sd(dprime_Intercept)                                         0.75     1.52 1.00
#> sd(dprime_condition)                                         0.48     0.99 1.01
#> sd(c_Intercept)                                              0.80     1.42 1.01
#> sd(c_condition)                                              0.48     0.85 1.01
#> sd(metac2zero1diff_Intercept)                                0.00     0.24 1.00
#> sd(metac2zero1diff_condition)                                0.00     0.14 1.00
#> sd(metac2zero2diff_Intercept)                                0.01     0.49 1.01
#> sd(metac2zero2diff_condition)                                0.00     0.27 1.01
#> sd(metac2zero3diff_Intercept)                                0.00     0.33 1.00
#> sd(metac2zero3diff_condition)                                0.00     0.23 1.01
#> sd(metac2one1diff_Intercept)                                 0.00     0.37 1.00
#> sd(metac2one1diff_condition)                                 0.00     0.23 1.00
#> sd(metac2one2diff_Intercept)                                 0.00     0.46 1.01
#> sd(metac2one2diff_condition)                                 0.00     0.25 1.01
#> sd(metac2one3diff_Intercept)                                 0.00     0.37 1.00
#> sd(metac2one3diff_condition)                                 0.00     0.23 1.00
#> cor(Intercept,condition)                                    -1.00     0.35 1.00
#> cor(dprime_Intercept,dprime_condition)                      -0.97    -0.80 1.00
#> cor(c_Intercept,c_condition)                                -0.97    -0.87 1.00
#> cor(metac2zero1diff_Intercept,metac2zero1diff_condition)    -0.99     0.92 1.00
#> cor(metac2zero2diff_Intercept,metac2zero2diff_condition)    -0.98     0.89 1.00
#> cor(metac2zero3diff_Intercept,metac2zero3diff_condition)    -0.99     0.88 1.00
#> cor(metac2one1diff_Intercept,metac2one1diff_condition)      -0.99     0.88 1.00
#> cor(metac2one2diff_Intercept,metac2one2diff_condition)      -0.99     0.91 1.00
#> cor(metac2one3diff_Intercept,metac2one3diff_condition)      -0.99     0.91 1.00
#>                                                          Bulk_ESS Tail_ESS
#> sd(Intercept)                                                 797     1315
#> sd(condition)                                                 638     1031
#> sd(dprime_Intercept)                                          854     1632
#> sd(dprime_condition)                                          727     1268
#> sd(c_Intercept)                                               682     1201
#> sd(c_condition)                                               633     1217
#> sd(metac2zero1diff_Intercept)                                1935     2040
#> sd(metac2zero1diff_condition)                                1336     1967
#> sd(metac2zero2diff_Intercept)                                1082     1683
#> sd(metac2zero2diff_condition)                                 663     1363
#> sd(metac2zero3diff_Intercept)                                1598     1781
#> sd(metac2zero3diff_condition)                                 851     1055
#> sd(metac2one1diff_Intercept)                                 1137     1385
#> sd(metac2one1diff_condition)                                  990     1478
#> sd(metac2one2diff_Intercept)                                  939     1206
#> sd(metac2one2diff_condition)                                 1027     1483
#> sd(metac2one3diff_Intercept)                                 1335     1700
#> sd(metac2one3diff_condition)                                 1206     1682
#> cor(Intercept,condition)                                     1092     1296
#> cor(dprime_Intercept,dprime_condition)                        926     1355
#> cor(c_Intercept,c_condition)                                  830     1400
#> cor(metac2zero1diff_Intercept,metac2zero1diff_condition)     2031     2090
#> cor(metac2zero2diff_Intercept,metac2zero2diff_condition)     1444     2283
#> cor(metac2zero3diff_Intercept,metac2zero3diff_condition)     1395     2248
#> cor(metac2one1diff_Intercept,metac2one1diff_condition)       1498     1891
#> cor(metac2one2diff_Intercept,metac2one2diff_condition)       1462     1996
#> cor(metac2one3diff_Intercept,metac2one3diff_condition)       2167     2584
#> 
#> Regression Coefficients:
#>                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> Intercept                     0.28      0.26    -0.25     0.75 1.00     1575
#> dprime_Intercept              1.55      0.26     1.04     2.06 1.01      756
#> c_Intercept                  -0.23      0.22    -0.69     0.19 1.01      381
#> metac2zero1diff_Intercept    -0.98      0.13    -1.23    -0.72 1.00     3950
#> metac2zero2diff_Intercept    -1.08      0.14    -1.37    -0.81 1.00     4084
#> metac2zero3diff_Intercept    -0.84      0.14    -1.12    -0.58 1.00     4460
#> metac2one1diff_Intercept     -0.94      0.13    -1.19    -0.70 1.00     4118
#> metac2one2diff_Intercept     -1.02      0.14    -1.32    -0.76 1.00     4238
#> metac2one3diff_Intercept     -1.11      0.16    -1.43    -0.79 1.00     3752
#> condition                    -0.15      0.16    -0.46     0.17 1.00     1760
#> dprime_condition             -0.36      0.17    -0.68    -0.03 1.01      679
#> c_condition                   0.21      0.13    -0.05     0.48 1.01      413
#> metac2zero1diff_condition     0.00      0.08    -0.16     0.15 1.00     3794
#> metac2zero2diff_condition     0.03      0.09    -0.14     0.21 1.00     4738
#> metac2zero3diff_condition    -0.06      0.09    -0.23     0.11 1.00     4495
#> metac2one1diff_condition     -0.03      0.08    -0.19     0.13 1.00     3848
#> metac2one2diff_condition      0.01      0.09    -0.16     0.19 1.00     4309
#> metac2one3diff_condition     -0.02      0.11    -0.23     0.18 1.00     3267
#>                           Tail_ESS
#> Intercept                     1897
#> dprime_Intercept              1224
#> c_Intercept                    395
#> metac2zero1diff_Intercept     1780
#> metac2zero2diff_Intercept     3024
#> metac2zero3diff_Intercept     2817
#> metac2one1diff_Intercept      2222
#> metac2one2diff_Intercept      2412
#> metac2one3diff_Intercept      3047
#> condition                     2075
#> dprime_condition               904
#> c_condition                    688
#> metac2zero1diff_condition     1790
#> metac2zero2diff_condition     2585
#> metac2zero3diff_condition     2921
#> metac2one1diff_condition      2189
#> metac2one2diff_condition      2501
#> metac2one3diff_condition      2563
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


<!-- README.md is generated from README.Rmd. Please edit that file -->

# hmetad

<!-- badges: start -->

[![R-CMD-check](https://github.com/metacoglab/hmetad/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/metacoglab/hmetad/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `hmetad` package is designed to fit the meta-d’ model for confidence
ratings ([Maniscalco & Lau, 2012](#ref-maniscalco2012),
[2014](#ref-maniscalco2014)). Like the [Hmeta-d
toolbox](https://github.com/metacoglab/HMeta-d) ([Fleming,
2017](#ref-fleming2017)), the `hmetad` package uses a Bayesian modeling
approach. The `hmetad` package builds on the Hmeta-d toolbox through
implementation as a custom family in the
[brms](https://paulbuerkner.com/brms/) package, which itself provides a
friendly interface to the probabilistic programming language
[Stan](https://mc-stan.org).

This provides major benefits:

- Model designs can be specified as simple `R` formulas
- Support for complex model designs (e.g., multilevel models,
  distributional models, multivariate models)
- Interfaces to other packages surrounding `brms` (e.g., `tidybayes`,
  `ggdist`, `bayesplot`, `loo`, `posterior`, `bridgesampling`,
  `bayestestR`)
- Computation of model-implied quantities (e.g., mean confidence, type 1
  and type 2 receiver operating characteristic curves, metacognitive
  bias)
- Increased sampling efficiency and better convergence diagnostics

## Installation

`hmetad` is currently under submission to CRAN, which means soon you
will be able to install it using:

``` r
install.packages("hmetad")
```

For now, you can install the development version of `hmetad` from
[GitHub](https://github.com/metacoglab/hmetad) with:

``` r
# install.packages("pak")
pak::pak("metacoglab/hmetad")
```

## Quick setup

Let’s say you have some data from a binary decision task with ordinal
confidence ratings:

    #> # A tibble: 1,000 × 5
    #>    trial stimulus response correct confidence
    #>    <int>    <int>    <int>   <int>      <int>
    #>  1     1        1        1       1          2
    #>  2     2        1        0       0          1
    #>  3     3        0        0       1          2
    #>  4     4        0        0       1          4
    #>  5     5        1        1       1          2
    #>  6     6        1        1       1          3
    #>  7     7        1        1       1          2
    #>  8     8        1        1       1          2
    #>  9     9        1        1       1          2
    #> 10    10        1        0       0          2
    #> # ℹ 990 more rows

You can fit an intercepts-only meta-d’ model using `fit_metad`:

``` r
library(hmetad)

m <- fit_metad(N ~ 1, data = d, file = "vignettes/models/readme1.rds")
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
    #> Intercept    -0.18      0.16    -0.52     0.11 1.00     3142     3260
    #> 
    #> Further Distributional Parameters:
    #>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> dprime              1.08      0.08     0.92     1.25 1.00     4527     3273
    #> c                   0.02      0.04    -0.06     0.10 1.00     3675     3067
    #> metac2zero1diff     0.48      0.03     0.41     0.55 1.00     4490     3209
    #> metac2zero2diff     0.52      0.04     0.44     0.60 1.00     5076     3242
    #> metac2zero3diff     0.55      0.05     0.46     0.65 1.00     5655     2716
    #> metac2one1diff      0.46      0.03     0.39     0.52 1.00     4627     2976
    #> metac2one2diff      0.43      0.04     0.36     0.50 1.00     4979     2987
    #> metac2one3diff      0.52      0.05     0.43     0.61 1.00     5653     2723
    #> 
    #> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    #> and Tail_ESS are effective sample size measures, and Rhat is the potential
    #> scale reduction factor on split chains (at convergence, Rhat = 1).

Now let’s say you have a more complicated design, such as a
within-participant manipulation:

    #> # A tibble: 5,000 × 7
    #> # Groups:   participant, condition [50]
    #>    participant condition trial stimulus response correct confidence
    #>          <int>     <int> <int>    <int>    <int>   <int>      <int>
    #>  1           1         1     1        0        0       1          4
    #>  2           1         1     2        1        0       0          3
    #>  3           1         1     3        1        0       0          4
    #>  4           1         1     4        1        1       1          3
    #>  5           1         1     5        0        0       1          4
    #>  6           1         1     6        0        0       1          1
    #>  7           1         1     7        0        1       0          2
    #>  8           1         1     8        0        0       1          3
    #>  9           1         1     9        1        1       1          3
    #> 10           1         1    10        1        0       0          1
    #> # ℹ 4,990 more rows

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
  data = d, init = "0", file = "vignettes/models/readme2.rds",
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
    #> sd(Intercept)                                                 0.53      0.16
    #> sd(condition2)                                                0.85      0.25
    #> sd(dprime_Intercept)                                          0.48      0.09
    #> sd(dprime_condition2)                                         0.67      0.14
    #> sd(c_Intercept)                                               0.64      0.10
    #> sd(c_condition2)                                              0.75      0.12
    #> sd(metac2zero1diff_Intercept)                                 0.09      0.06
    #> sd(metac2zero1diff_condition2)                                0.09      0.07
    #> sd(metac2zero2diff_Intercept)                                 0.15      0.09
    #> sd(metac2zero2diff_condition2)                                0.17      0.11
    #> sd(metac2zero3diff_Intercept)                                 0.07      0.06
    #> sd(metac2zero3diff_condition2)                                0.14      0.10
    #> sd(metac2one1diff_Intercept)                                  0.08      0.05
    #> sd(metac2one1diff_condition2)                                 0.12      0.08
    #> sd(metac2one2diff_Intercept)                                  0.12      0.07
    #> sd(metac2one2diff_condition2)                                 0.10      0.07
    #> sd(metac2one3diff_Intercept)                                  0.21      0.12
    #> sd(metac2one3diff_condition2)                                 0.19      0.13
    #> cor(Intercept,condition2)                                    -0.84      0.15
    #> cor(dprime_Intercept,dprime_condition2)                      -0.61      0.16
    #> cor(c_Intercept,c_condition2)                                -0.79      0.08
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)    -0.10      0.57
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)    -0.20      0.56
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)    -0.23      0.58
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)      -0.07      0.59
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)       0.05      0.56
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)      -0.63      0.46
    #>                                                           l-95% CI u-95% CI
    #> sd(Intercept)                                                 0.28     0.89
    #> sd(condition2)                                                0.40     1.39
    #> sd(dprime_Intercept)                                          0.33     0.68
    #> sd(dprime_condition2)                                         0.45     0.98
    #> sd(c_Intercept)                                               0.48     0.86
    #> sd(c_condition2)                                              0.55     1.02
    #> sd(metac2zero1diff_Intercept)                                 0.00     0.22
    #> sd(metac2zero1diff_condition2)                                0.00     0.25
    #> sd(metac2zero2diff_Intercept)                                 0.01     0.34
    #> sd(metac2zero2diff_condition2)                                0.01     0.42
    #> sd(metac2zero3diff_Intercept)                                 0.00     0.21
    #> sd(metac2zero3diff_condition2)                                0.01     0.37
    #> sd(metac2one1diff_Intercept)                                  0.00     0.20
    #> sd(metac2one1diff_condition2)                                 0.01     0.31
    #> sd(metac2one2diff_Intercept)                                  0.01     0.26
    #> sd(metac2one2diff_condition2)                                 0.00     0.27
    #> sd(metac2one3diff_Intercept)                                  0.02     0.46
    #> sd(metac2one3diff_condition2)                                 0.01     0.47
    #> cor(Intercept,condition2)                                    -0.99    -0.44
    #> cor(dprime_Intercept,dprime_condition2)                      -0.85    -0.25
    #> cor(c_Intercept,c_condition2)                                -0.91    -0.59
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)    -0.96     0.93
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)    -0.97     0.93
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)    -0.98     0.91
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)      -0.96     0.95
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)      -0.92     0.95
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)      -1.00     0.70
    #>                                                           Rhat Bulk_ESS
    #> sd(Intercept)                                             1.00     1976
    #> sd(condition2)                                            1.00     1316
    #> sd(dprime_Intercept)                                      1.00     1744
    #> sd(dprime_condition2)                                     1.00     1306
    #> sd(c_Intercept)                                           1.00      940
    #> sd(c_condition2)                                          1.00      908
    #> sd(metac2zero1diff_Intercept)                             1.00     1721
    #> sd(metac2zero1diff_condition2)                            1.00     1858
    #> sd(metac2zero2diff_Intercept)                             1.00     1372
    #> sd(metac2zero2diff_condition2)                            1.00     1294
    #> sd(metac2zero3diff_Intercept)                             1.00     1676
    #> sd(metac2zero3diff_condition2)                            1.00     1669
    #> sd(metac2one1diff_Intercept)                              1.00     2318
    #> sd(metac2one1diff_condition2)                             1.00     1592
    #> sd(metac2one2diff_Intercept)                              1.00     1303
    #> sd(metac2one2diff_condition2)                             1.00     1848
    #> sd(metac2one3diff_Intercept)                              1.00      863
    #> sd(metac2one3diff_condition2)                             1.00     1038
    #> cor(Intercept,condition2)                                 1.00     1379
    #> cor(dprime_Intercept,dprime_condition2)                   1.00     1635
    #> cor(c_Intercept,c_condition2)                             1.00     1090
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2) 1.00     4554
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2) 1.00     2823
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2) 1.00     2675
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)   1.00     2354
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)   1.00     4178
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)   1.00     1520
    #>                                                           Tail_ESS
    #> sd(Intercept)                                                 2725
    #> sd(condition2)                                                1774
    #> sd(dprime_Intercept)                                          2325
    #> sd(dprime_condition2)                                         2546
    #> sd(c_Intercept)                                               1722
    #> sd(c_condition2)                                              1190
    #> sd(metac2zero1diff_Intercept)                                 2037
    #> sd(metac2zero1diff_condition2)                                1691
    #> sd(metac2zero2diff_Intercept)                                 1808
    #> sd(metac2zero2diff_condition2)                                1908
    #> sd(metac2zero3diff_Intercept)                                 2164
    #> sd(metac2zero3diff_condition2)                                2429
    #> sd(metac2one1diff_Intercept)                                  2459
    #> sd(metac2one1diff_condition2)                                 1920
    #> sd(metac2one2diff_Intercept)                                  1510
    #> sd(metac2one2diff_condition2)                                 2032
    #> sd(metac2one3diff_Intercept)                                  1528
    #> sd(metac2one3diff_condition2)                                 1590
    #> cor(Intercept,condition2)                                     2113
    #> cor(dprime_Intercept,dprime_condition2)                       2358
    #> cor(c_Intercept,c_condition2)                                 1538
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)     2956
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)     2275
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)     2654
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)       2669
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)       3249
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)       2395
    #> 
    #> Regression Coefficients:
    #>                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    #> Intercept                     -0.10      0.16    -0.44     0.19 1.00     3095
    #> dprime_Intercept               0.90      0.11     0.67     1.12 1.00     1523
    #> c_Intercept                   -0.02      0.13    -0.27     0.24 1.01      474
    #> metac2zero1diff_Intercept     -0.95      0.06    -1.07    -0.84 1.00     5998
    #> metac2zero2diff_Intercept     -1.02      0.07    -1.16    -0.89 1.00     4764
    #> metac2zero3diff_Intercept     -1.05      0.07    -1.18    -0.92 1.00     6306
    #> metac2one1diff_Intercept      -0.96      0.06    -1.09    -0.85 1.00     5967
    #> metac2one2diff_Intercept      -0.97      0.06    -1.10    -0.84 1.00     5015
    #> metac2one3diff_Intercept      -1.09      0.08    -1.26    -0.93 1.00     4329
    #> condition2                    -0.05      0.24    -0.53     0.43 1.00     2754
    #> dprime_condition2              0.03      0.15    -0.27     0.33 1.00     2083
    #> c_condition2                  -0.19      0.15    -0.48     0.11 1.00      809
    #> metac2zero1diff_condition2    -0.09      0.08    -0.25     0.07 1.00     5915
    #> metac2zero2diff_condition2    -0.14      0.10    -0.32     0.05 1.00     4944
    #> metac2zero3diff_condition2     0.01      0.10    -0.17     0.20 1.00     5620
    #> metac2one1diff_condition2     -0.12      0.08    -0.28     0.04 1.00     5205
    #> metac2one2diff_condition2     -0.01      0.08    -0.17     0.15 1.00     5177
    #> metac2one3diff_condition2      0.10      0.10    -0.09     0.31 1.00     5508
    #>                            Tail_ESS
    #> Intercept                      2616
    #> dprime_Intercept               2172
    #> c_Intercept                    1239
    #> metac2zero1diff_Intercept      2829
    #> metac2zero2diff_Intercept      3016
    #> metac2zero3diff_Intercept      3005
    #> metac2one1diff_Intercept       2897
    #> metac2one2diff_Intercept       2952
    #> metac2one3diff_Intercept       2879
    #> condition2                     2771
    #> dprime_condition2              2679
    #> c_condition2                   1446
    #> metac2zero1diff_condition2     2905
    #> metac2zero2diff_condition2     3129
    #> metac2zero3diff_condition2     2461
    #> metac2one1diff_condition2      2420
    #> metac2one2diff_condition2      2707
    #> metac2one3diff_condition2      3413
    #> 
    #> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    #> and Tail_ESS are effective sample size measures, and Rhat is the potential
    #> scale reduction factor on split chains (at convergence, Rhat = 1).

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0" line-spacing="2">

<div id="ref-fleming2017" class="csl-entry">

Fleming, S. M. (2017). HMeta-d: Hierarchical bayesian estimation of
metacognitive efficiency from confidence ratings. *Neuroscience of
Consciousness*, *2017*(1), nix007.

</div>

<div id="ref-maniscalco2012" class="csl-entry">

Maniscalco, B., & Lau, H. (2012). A signal detection theoretic approach
for estimating metacognitive sensitivity from confidence ratings.
*Consciousness and Cognition*, *21*(1), 422–430.

</div>

<div id="ref-maniscalco2014" class="csl-entry">

Maniscalco, B., & Lau, H. (2014). Signal detection theory analysis of
type 1 and type 2 data: Meta-d′, response-specific meta-d′, and the
unequal variance SDT model. In *The cognitive neuroscience of
metacognition* (pp. 25–66). Springer.

</div>

</div>

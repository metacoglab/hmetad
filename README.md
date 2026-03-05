
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hmetad

<!-- badges: start -->

[![R-CMD-check](https://github.com/metacoglab/hmetad/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/metacoglab/hmetad/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

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
    #>  1     1        1        1       1          1
    #>  2     2        1        1       1          4
    #>  3     3        0        1       0          2
    #>  4     4        1        0       0          3
    #>  5     5        1        1       1          3
    #>  6     6        0        1       0          1
    #>  7     7        0        0       1          1
    #>  8     8        1        0       0          2
    #>  9     9        0        1       0          1
    #> 10    10        1        1       1          2
    #> # ℹ 990 more rows

You can fit an intercepts-only meta-d’ model using `fit_metad`:

``` r
library(hmetad)

m <- fit_metad(N ~ 1, data = d)
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
    #> Intercept    -0.06      0.15    -0.37     0.22 1.00     3499     3160
    #> 
    #> Further Distributional Parameters:
    #>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> dprime              1.08      0.08     0.92     1.25 1.00     4034     3225
    #> c                  -0.05      0.04    -0.13     0.03 1.00     4026     3038
    #> metac2zero1diff     0.50      0.04     0.44     0.57 1.00     5219     3390
    #> metac2zero2diff     0.48      0.04     0.41     0.56 1.00     5492     3136
    #> metac2zero3diff     0.46      0.05     0.37     0.55 1.00     5509     3055
    #> metac2one1diff      0.54      0.04     0.46     0.61 1.00     4859     3329
    #> metac2one2diff      0.54      0.04     0.47     0.62 1.00     5115     2993
    #> metac2one3diff      0.49      0.05     0.40     0.58 1.00     6067     3040
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
    #>  1           1         1     1        0        0       1          2
    #>  2           1         1     2        0        0       1          4
    #>  3           1         1     3        0        1       0          4
    #>  4           1         1     4        0        1       0          1
    #>  5           1         1     5        0        1       0          3
    #>  6           1         1     6        1        1       1          4
    #>  7           1         1     7        0        0       1          2
    #>  8           1         1     8        1        0       0          1
    #>  9           1         1     9        0        0       1          1
    #> 10           1         1    10        1        0       0          2
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
    #> sd(Intercept)                                                 0.56      0.17
    #> sd(condition2)                                                0.66      0.22
    #> sd(dprime_Intercept)                                          0.36      0.08
    #> sd(dprime_condition2)                                         0.62      0.12
    #> sd(c_Intercept)                                               0.53      0.09
    #> sd(c_condition2)                                              0.58      0.10
    #> sd(metac2zero1diff_Intercept)                                 0.12      0.07
    #> sd(metac2zero1diff_condition2)                                0.11      0.08
    #> sd(metac2zero2diff_Intercept)                                 0.12      0.08
    #> sd(metac2zero2diff_condition2)                                0.40      0.13
    #> sd(metac2zero3diff_Intercept)                                 0.12      0.08
    #> sd(metac2zero3diff_condition2)                                0.15      0.10
    #> sd(metac2one1diff_Intercept)                                  0.07      0.05
    #> sd(metac2one1diff_condition2)                                 0.09      0.07
    #> sd(metac2one2diff_Intercept)                                  0.10      0.07
    #> sd(metac2one2diff_condition2)                                 0.12      0.09
    #> sd(metac2one3diff_Intercept)                                  0.12      0.08
    #> sd(metac2one3diff_condition2)                                 0.15      0.11
    #> cor(Intercept,condition2)                                    -0.16      0.37
    #> cor(dprime_Intercept,dprime_condition2)                      -0.37      0.24
    #> cor(c_Intercept,c_condition2)                                -0.50      0.16
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)    -0.18      0.56
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)    -0.31      0.50
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)    -0.05      0.57
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)      -0.03      0.57
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)      -0.26      0.57
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)      -0.28      0.57
    #>                                                           l-95% CI u-95% CI
    #> sd(Intercept)                                                 0.27     0.95
    #> sd(condition2)                                                0.28     1.14
    #> sd(dprime_Intercept)                                          0.21     0.54
    #> sd(dprime_condition2)                                         0.40     0.89
    #> sd(c_Intercept)                                               0.39     0.73
    #> sd(c_condition2)                                              0.42     0.81
    #> sd(metac2zero1diff_Intercept)                                 0.01     0.27
    #> sd(metac2zero1diff_condition2)                                0.01     0.30
    #> sd(metac2zero2diff_Intercept)                                 0.01     0.30
    #> sd(metac2zero2diff_condition2)                                0.14     0.68
    #> sd(metac2zero3diff_Intercept)                                 0.01     0.31
    #> sd(metac2zero3diff_condition2)                                0.01     0.38
    #> sd(metac2one1diff_Intercept)                                  0.00     0.20
    #> sd(metac2one1diff_condition2)                                 0.00     0.26
    #> sd(metac2one2diff_Intercept)                                  0.01     0.26
    #> sd(metac2one2diff_condition2)                                 0.00     0.33
    #> sd(metac2one3diff_Intercept)                                  0.01     0.31
    #> sd(metac2one3diff_condition2)                                 0.01     0.41
    #> cor(Intercept,condition2)                                    -0.74     0.71
    #> cor(dprime_Intercept,dprime_condition2)                      -0.75     0.16
    #> cor(c_Intercept,c_condition2)                                -0.77    -0.14
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)    -0.96     0.92
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)    -0.96     0.86
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)    -0.95     0.95
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)      -0.94     0.94
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)      -0.98     0.93
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)      -0.98     0.90
    #>                                                           Rhat Bulk_ESS
    #> sd(Intercept)                                             1.00     1615
    #> sd(condition2)                                            1.00     1167
    #> sd(dprime_Intercept)                                      1.00     1769
    #> sd(dprime_condition2)                                     1.01     1194
    #> sd(c_Intercept)                                           1.00     1025
    #> sd(c_condition2)                                          1.00     1036
    #> sd(metac2zero1diff_Intercept)                             1.00     1467
    #> sd(metac2zero1diff_condition2)                            1.00     1954
    #> sd(metac2zero2diff_Intercept)                             1.00     1642
    #> sd(metac2zero2diff_condition2)                            1.01     1010
    #> sd(metac2zero3diff_Intercept)                             1.00     1229
    #> sd(metac2zero3diff_condition2)                            1.00     1477
    #> sd(metac2one1diff_Intercept)                              1.00     1844
    #> sd(metac2one1diff_condition2)                             1.00     1926
    #> sd(metac2one2diff_Intercept)                              1.00     1966
    #> sd(metac2one2diff_condition2)                             1.00     1637
    #> sd(metac2one3diff_Intercept)                              1.00     1478
    #> sd(metac2one3diff_condition2)                             1.00     1347
    #> cor(Intercept,condition2)                                 1.00     1144
    #> cor(dprime_Intercept,dprime_condition2)                   1.00     1085
    #> cor(c_Intercept,c_condition2)                             1.00     1223
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2) 1.00     3940
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2) 1.01      393
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2) 1.00     2608
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)   1.00     3472
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)   1.00     2784
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)   1.00     2769
    #>                                                           Tail_ESS
    #> sd(Intercept)                                                 2278
    #> sd(condition2)                                                1380
    #> sd(dprime_Intercept)                                          2801
    #> sd(dprime_condition2)                                         2036
    #> sd(c_Intercept)                                               1600
    #> sd(c_condition2)                                              1772
    #> sd(metac2zero1diff_Intercept)                                 1698
    #> sd(metac2zero1diff_condition2)                                2374
    #> sd(metac2zero2diff_Intercept)                                 1932
    #> sd(metac2zero2diff_condition2)                                1397
    #> sd(metac2zero3diff_Intercept)                                 2100
    #> sd(metac2zero3diff_condition2)                                1730
    #> sd(metac2one1diff_Intercept)                                  2428
    #> sd(metac2one1diff_condition2)                                 1522
    #> sd(metac2one2diff_Intercept)                                  2451
    #> sd(metac2one2diff_condition2)                                 2009
    #> sd(metac2one3diff_Intercept)                                  1322
    #> sd(metac2one3diff_condition2)                                 1660
    #> cor(Intercept,condition2)                                     1059
    #> cor(dprime_Intercept,dprime_condition2)                       1750
    #> cor(c_Intercept,c_condition2)                                 1782
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)     2843
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)      999
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)     2383
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)       2659
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)       2700
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)       3032
    #> 
    #> Regression Coefficients:
    #>                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    #> Intercept                      0.12      0.17    -0.25     0.42 1.00     2319
    #> dprime_Intercept               0.88      0.09     0.70     1.06 1.00     2760
    #> c_Intercept                   -0.12      0.11    -0.33     0.11 1.00      577
    #> metac2zero1diff_Intercept     -1.04      0.06    -1.16    -0.92 1.00     5908
    #> metac2zero2diff_Intercept     -0.99      0.07    -1.13    -0.86 1.00     5749
    #> metac2zero3diff_Intercept     -1.01      0.08    -1.17    -0.87 1.00     5687
    #> metac2one1diff_Intercept      -1.01      0.06    -1.13    -0.89 1.00     6546
    #> metac2one2diff_Intercept      -0.93      0.06    -1.05    -0.81 1.00     6844
    #> metac2one3diff_Intercept      -1.10      0.07    -1.24    -0.96 1.00     5729
    #> condition2                    -0.02      0.22    -0.48     0.39 1.00     2216
    #> dprime_condition2              0.17      0.14    -0.11     0.45 1.00     2108
    #> c_condition2                  -0.10      0.12    -0.34     0.14 1.00      675
    #> metac2zero1diff_condition2     0.05      0.09    -0.12     0.22 1.00     5745
    #> metac2zero2diff_condition2     0.09      0.12    -0.15     0.33 1.00     3428
    #> metac2zero3diff_condition2     0.10      0.11    -0.12     0.31 1.00     5774
    #> metac2one1diff_condition2      0.07      0.08    -0.09     0.23 1.00     6787
    #> metac2one2diff_condition2     -0.13      0.09    -0.30     0.04 1.00     6040
    #> metac2one3diff_condition2      0.03      0.10    -0.16     0.23 1.00     6259
    #>                            Tail_ESS
    #> Intercept                      2856
    #> dprime_Intercept               2966
    #> c_Intercept                    1166
    #> metac2zero1diff_Intercept      3189
    #> metac2zero2diff_Intercept      2959
    #> metac2zero3diff_Intercept      3199
    #> metac2one1diff_Intercept       2716
    #> metac2one2diff_Intercept       2893
    #> metac2one3diff_Intercept       3484
    #> condition2                     2325
    #> dprime_condition2              2670
    #> c_condition2                   1246
    #> metac2zero1diff_condition2     2892
    #> metac2zero2diff_condition2     3034
    #> metac2zero3diff_condition2     2857
    #> metac2one1diff_condition2      2743
    #> metac2one2diff_condition2      3031
    #> metac2one3diff_condition2      3001
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

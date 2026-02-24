
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
    #>  1     1        1        0       0          3
    #>  2     2        1        1       1          2
    #>  3     3        1        1       1          4
    #>  4     4        1        1       1          1
    #>  5     5        1        1       1          4
    #>  6     6        0        0       1          1
    #>  7     7        1        0       0          2
    #>  8     8        1        1       1          2
    #>  9     9        0        0       1          3
    #> 10    10        1        1       1          3
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
    #> Intercept     0.00      0.15    -0.30     0.28 1.00     3061     2726
    #> 
    #> Further Distributional Parameters:
    #>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> dprime              1.01      0.08     0.84     1.17 1.00     3858     3126
    #> c                   0.02      0.04    -0.06     0.10 1.00     3950     2891
    #> metac2zero1diff     0.52      0.04     0.45     0.60 1.00     4451     3184
    #> metac2zero2diff     0.47      0.04     0.40     0.55 1.00     5156     2827
    #> metac2zero3diff     0.50      0.05     0.41     0.59 1.00     4886     3072
    #> metac2one1diff      0.45      0.03     0.38     0.51 1.00     5075     3382
    #> metac2one2diff      0.48      0.04     0.40     0.55 1.00     4820     3042
    #> metac2one3diff      0.53      0.05     0.44     0.62 1.00     6408     3018
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
    #>  2           1         1     2        1        1       1          4
    #>  3           1         1     3        0        0       1          4
    #>  4           1         1     4        1        0       0          1
    #>  5           1         1     5        0        1       0          1
    #>  6           1         1     6        1        0       0          1
    #>  7           1         1     7        1        0       0          1
    #>  8           1         1     8        1        1       1          4
    #>  9           1         1     9        0        0       1          4
    #> 10           1         1    10        0        0       1          4
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
    #> sd(Intercept)                                                 0.17      0.12
    #> sd(condition2)                                                0.69      0.21
    #> sd(dprime_Intercept)                                          0.38      0.08
    #> sd(dprime_condition2)                                         0.76      0.14
    #> sd(c_Intercept)                                               0.66      0.10
    #> sd(c_condition2)                                              0.93      0.14
    #> sd(metac2zero1diff_Intercept)                                 0.07      0.06
    #> sd(metac2zero1diff_condition2)                                0.10      0.08
    #> sd(metac2zero2diff_Intercept)                                 0.11      0.08
    #> sd(metac2zero2diff_condition2)                                0.22      0.13
    #> sd(metac2zero3diff_Intercept)                                 0.09      0.06
    #> sd(metac2zero3diff_condition2)                                0.11      0.09
    #> sd(metac2one1diff_Intercept)                                  0.08      0.06
    #> sd(metac2one1diff_condition2)                                 0.15      0.10
    #> sd(metac2one2diff_Intercept)                                  0.06      0.05
    #> sd(metac2one2diff_condition2)                                 0.24      0.13
    #> sd(metac2one3diff_Intercept)                                  0.15      0.09
    #> sd(metac2one3diff_condition2)                                 0.14      0.10
    #> cor(Intercept,condition2)                                    -0.29      0.47
    #> cor(dprime_Intercept,dprime_condition2)                      -0.59      0.17
    #> cor(c_Intercept,c_condition2)                                -0.71      0.11
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)    -0.30      0.58
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)    -0.49      0.52
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)    -0.19      0.57
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)      -0.17      0.58
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)      -0.11      0.58
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)       0.10      0.56
    #>                                                           l-95% CI u-95% CI
    #> sd(Intercept)                                                 0.01     0.44
    #> sd(condition2)                                                0.37     1.21
    #> sd(dprime_Intercept)                                          0.25     0.55
    #> sd(dprime_condition2)                                         0.53     1.07
    #> sd(c_Intercept)                                               0.49     0.89
    #> sd(c_condition2)                                              0.70     1.25
    #> sd(metac2zero1diff_Intercept)                                 0.00     0.22
    #> sd(metac2zero1diff_condition2)                                0.00     0.31
    #> sd(metac2zero2diff_Intercept)                                 0.00     0.29
    #> sd(metac2zero2diff_condition2)                                0.02     0.51
    #> sd(metac2zero3diff_Intercept)                                 0.00     0.24
    #> sd(metac2zero3diff_condition2)                                0.00     0.31
    #> sd(metac2one1diff_Intercept)                                  0.00     0.22
    #> sd(metac2one1diff_condition2)                                 0.01     0.38
    #> sd(metac2one2diff_Intercept)                                  0.00     0.18
    #> sd(metac2one2diff_condition2)                                 0.02     0.52
    #> sd(metac2one3diff_Intercept)                                  0.01     0.33
    #> sd(metac2one3diff_condition2)                                 0.01     0.36
    #> cor(Intercept,condition2)                                    -0.95     0.78
    #> cor(dprime_Intercept,dprime_condition2)                      -0.85    -0.22
    #> cor(c_Intercept,c_condition2)                                -0.88    -0.47
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)    -0.99     0.89
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)    -0.99     0.81
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)    -0.97     0.91
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)      -0.97     0.93
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)      -0.96     0.94
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)      -0.92     0.95
    #>                                                           Rhat Bulk_ESS
    #> sd(Intercept)                                             1.00      894
    #> sd(condition2)                                            1.00      881
    #> sd(dprime_Intercept)                                      1.00     1591
    #> sd(dprime_condition2)                                     1.00     1070
    #> sd(c_Intercept)                                           1.00      695
    #> sd(c_condition2)                                          1.00      686
    #> sd(metac2zero1diff_Intercept)                             1.00     1307
    #> sd(metac2zero1diff_condition2)                            1.00     1289
    #> sd(metac2zero2diff_Intercept)                             1.00      987
    #> sd(metac2zero2diff_condition2)                            1.00      719
    #> sd(metac2zero3diff_Intercept)                             1.00     1332
    #> sd(metac2zero3diff_condition2)                            1.00     1132
    #> sd(metac2one1diff_Intercept)                              1.00     1552
    #> sd(metac2one1diff_condition2)                             1.01      766
    #> sd(metac2one2diff_Intercept)                              1.00     1951
    #> sd(metac2one2diff_condition2)                             1.00      818
    #> sd(metac2one3diff_Intercept)                              1.00     1196
    #> sd(metac2one3diff_condition2)                             1.00     1443
    #> cor(Intercept,condition2)                                 1.01      457
    #> cor(dprime_Intercept,dprime_condition2)                   1.00      677
    #> cor(c_Intercept,c_condition2)                             1.01      610
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2) 1.00     2033
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2) 1.00     1017
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2) 1.00     3046
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)   1.00     1288
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)   1.00      788
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)   1.00     2317
    #>                                                           Tail_ESS
    #> sd(Intercept)                                                 1943
    #> sd(condition2)                                                 483
    #> sd(dprime_Intercept)                                          2145
    #> sd(dprime_condition2)                                         2167
    #> sd(c_Intercept)                                               1344
    #> sd(c_condition2)                                              1493
    #> sd(metac2zero1diff_Intercept)                                 1966
    #> sd(metac2zero1diff_condition2)                                1732
    #> sd(metac2zero2diff_Intercept)                                 1638
    #> sd(metac2zero2diff_condition2)                                1191
    #> sd(metac2zero3diff_Intercept)                                 2255
    #> sd(metac2zero3diff_condition2)                                1212
    #> sd(metac2one1diff_Intercept)                                  1946
    #> sd(metac2one1diff_condition2)                                  508
    #> sd(metac2one2diff_Intercept)                                  1211
    #> sd(metac2one2diff_condition2)                                 1499
    #> sd(metac2one3diff_Intercept)                                  1495
    #> sd(metac2one3diff_condition2)                                 1986
    #> cor(Intercept,condition2)                                      992
    #> cor(dprime_Intercept,dprime_condition2)                       1722
    #> cor(c_Intercept,c_condition2)                                 1272
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)     2653
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)     2062
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)     3145
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)       2284
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)       1688
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)       2324
    #> 
    #> Regression Coefficients:
    #>                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    #> Intercept                      0.18      0.10    -0.03     0.37 1.00     3126
    #> dprime_Intercept               0.93      0.09     0.75     1.12 1.00     1744
    #> c_Intercept                    0.15      0.13    -0.12     0.40 1.00      447
    #> metac2zero1diff_Intercept     -0.97      0.06    -1.09    -0.87 1.00     5437
    #> metac2zero2diff_Intercept     -1.13      0.07    -1.27    -0.99 1.00     2138
    #> metac2zero3diff_Intercept     -0.91      0.06    -1.04    -0.79 1.00     5887
    #> metac2one1diff_Intercept      -1.04      0.06    -1.16    -0.92 1.00     4920
    #> metac2one2diff_Intercept      -0.98      0.06    -1.11    -0.86 1.00     6606
    #> metac2one3diff_Intercept      -1.06      0.08    -1.22    -0.91 1.00     2715
    #> condition2                    -0.22      0.21    -0.66     0.18 1.00     1489
    #> dprime_condition2              0.07      0.17    -0.26     0.40 1.00      844
    #> c_condition2                  -0.02      0.18    -0.37     0.34 1.00      536
    #> metac2zero1diff_condition2    -0.04      0.08    -0.20     0.14 1.00     5345
    #> metac2zero2diff_condition2     0.12      0.10    -0.08     0.32 1.00     2607
    #> metac2zero3diff_condition2    -0.18      0.09    -0.36    -0.00 1.00     4658
    #> metac2one1diff_condition2     -0.01      0.09    -0.20     0.17 1.00     4540
    #> metac2one2diff_condition2     -0.14      0.11    -0.36     0.07 1.00     2922
    #> metac2one3diff_condition2      0.00      0.11    -0.21     0.21 1.00     3172
    #>                            Tail_ESS
    #> Intercept                      2775
    #> dprime_Intercept               2402
    #> c_Intercept                     774
    #> metac2zero1diff_Intercept      3154
    #> metac2zero2diff_Intercept       655
    #> metac2zero3diff_Intercept      2420
    #> metac2one1diff_Intercept       3165
    #> metac2one2diff_Intercept       3128
    #> metac2one3diff_Intercept        596
    #> condition2                     1211
    #> dprime_condition2              1515
    #> c_condition2                   1152
    #> metac2zero1diff_condition2     2301
    #> metac2zero2diff_condition2      674
    #> metac2zero3diff_condition2     3009
    #> metac2one1diff_condition2      2980
    #> metac2one2diff_condition2      2464
    #> metac2one3diff_condition2      1546
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

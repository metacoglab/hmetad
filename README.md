
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

    #> # A tibble: 1,000 × 5
    #>    trial stimulus response correct confidence
    #>    <int>    <int>    <int>   <int>      <int>
    #>  1     1        1        0       0          2
    #>  2     2        1        1       1          3
    #>  3     3        0        0       1          4
    #>  4     4        0        0       1          4
    #>  5     5        1        0       0          4
    #>  6     6        1        1       1          2
    #>  7     7        0        0       1          4
    #>  8     8        1        1       1          2
    #>  9     9        1        0       0          1
    #> 10    10        0        1       0          3
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
    #> Intercept    -0.22      0.19    -0.63     0.12 1.00     3691     2740
    #> 
    #> Further Distributional Parameters:
    #>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> dprime              0.93      0.08     0.76     1.09 1.00     4055     2686
    #> c                  -0.06      0.04    -0.14     0.02 1.00     3623     3514
    #> metac2zero1diff     0.44      0.03     0.38     0.51 1.00     5115     3353
    #> metac2zero2diff     0.46      0.04     0.39     0.54 1.00     5804     2548
    #> metac2zero3diff     0.47      0.05     0.39     0.56 1.00     6643     3225
    #> metac2one1diff      0.47      0.03     0.40     0.53 1.00     5415     3358
    #> metac2one2diff      0.49      0.04     0.42     0.56 1.00     6503     3306
    #> metac2one3diff      0.53      0.05     0.45     0.63 1.00     6923     3127
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
    #>  1           1         1     1        0        1       0          3
    #>  2           1         1     2        1        1       1          2
    #>  3           1         1     3        0        0       1          1
    #>  4           1         1     4        1        1       1          4
    #>  5           1         1     5        0        0       1          3
    #>  6           1         1     6        0        1       0          3
    #>  7           1         1     7        1        1       1          1
    #>  8           1         1     8        1        1       1          1
    #>  9           1         1     9        0        1       0          3
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
    #> sd(Intercept)                                                 0.55      0.14
    #> sd(condition2)                                                0.47      0.20
    #> sd(dprime_Intercept)                                          0.57      0.10
    #> sd(dprime_condition2)                                         0.81      0.15
    #> sd(c_Intercept)                                               0.59      0.10
    #> sd(c_condition2)                                              0.72      0.12
    #> sd(metac2zero1diff_Intercept)                                 0.09      0.07
    #> sd(metac2zero1diff_condition2)                                0.20      0.12
    #> sd(metac2zero2diff_Intercept)                                 0.08      0.06
    #> sd(metac2zero2diff_condition2)                                0.12      0.09
    #> sd(metac2zero3diff_Intercept)                                 0.12      0.08
    #> sd(metac2zero3diff_condition2)                                0.19      0.12
    #> sd(metac2one1diff_Intercept)                                  0.09      0.06
    #> sd(metac2one1diff_condition2)                                 0.13      0.09
    #> sd(metac2one2diff_Intercept)                                  0.09      0.06
    #> sd(metac2one2diff_condition2)                                 0.16      0.11
    #> sd(metac2one3diff_Intercept)                                  0.07      0.05
    #> sd(metac2one3diff_condition2)                                 0.12      0.09
    #> cor(Intercept,condition2)                                    -0.35      0.35
    #> cor(dprime_Intercept,dprime_condition2)                      -0.75      0.12
    #> cor(c_Intercept,c_condition2)                                -0.58      0.15
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)    -0.22      0.57
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)    -0.13      0.58
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)    -0.05      0.55
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)      -0.20      0.57
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)      -0.22      0.56
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)      -0.21      0.57
    #>                                                           l-95% CI u-95% CI
    #> sd(Intercept)                                                 0.32     0.87
    #> sd(condition2)                                                0.10     0.89
    #> sd(dprime_Intercept)                                          0.41     0.79
    #> sd(dprime_condition2)                                         0.57     1.15
    #> sd(c_Intercept)                                               0.43     0.82
    #> sd(c_condition2)                                              0.53     0.99
    #> sd(metac2zero1diff_Intercept)                                 0.00     0.24
    #> sd(metac2zero1diff_condition2)                                0.01     0.45
    #> sd(metac2zero2diff_Intercept)                                 0.00     0.21
    #> sd(metac2zero2diff_condition2)                                0.00     0.33
    #> sd(metac2zero3diff_Intercept)                                 0.01     0.31
    #> sd(metac2zero3diff_condition2)                                0.01     0.44
    #> sd(metac2one1diff_Intercept)                                  0.00     0.23
    #> sd(metac2one1diff_condition2)                                 0.01     0.35
    #> sd(metac2one2diff_Intercept)                                  0.00     0.24
    #> sd(metac2one2diff_condition2)                                 0.01     0.40
    #> sd(metac2one3diff_Intercept)                                  0.00     0.20
    #> sd(metac2one3diff_condition2)                                 0.00     0.34
    #> cor(Intercept,condition2)                                    -0.85     0.49
    #> cor(dprime_Intercept,dprime_condition2)                      -0.91    -0.47
    #> cor(c_Intercept,c_condition2)                                -0.81    -0.25
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)    -0.97     0.92
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)    -0.97     0.94
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)    -0.94     0.92
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)      -0.97     0.91
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)      -0.97     0.92
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)      -0.98     0.91
    #>                                                           Rhat Bulk_ESS
    #> sd(Intercept)                                             1.00     2214
    #> sd(condition2)                                            1.00     1002
    #> sd(dprime_Intercept)                                      1.00     1324
    #> sd(dprime_condition2)                                     1.00     1824
    #> sd(c_Intercept)                                           1.00     1384
    #> sd(c_condition2)                                          1.00     1221
    #> sd(metac2zero1diff_Intercept)                             1.00     2729
    #> sd(metac2zero1diff_condition2)                            1.00     1057
    #> sd(metac2zero2diff_Intercept)                             1.00     2742
    #> sd(metac2zero2diff_condition2)                            1.00     2131
    #> sd(metac2zero3diff_Intercept)                             1.00     1615
    #> sd(metac2zero3diff_condition2)                            1.00     1068
    #> sd(metac2one1diff_Intercept)                              1.00     1702
    #> sd(metac2one1diff_condition2)                             1.00     1898
    #> sd(metac2one2diff_Intercept)                              1.00     1879
    #> sd(metac2one2diff_condition2)                             1.00     1476
    #> sd(metac2one3diff_Intercept)                              1.00     2503
    #> sd(metac2one3diff_condition2)                             1.00     1807
    #> cor(Intercept,condition2)                                 1.00     2511
    #> cor(dprime_Intercept,dprime_condition2)                   1.00     1910
    #> cor(c_Intercept,c_condition2)                             1.00     1211
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2) 1.00     1315
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2) 1.00     3771
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2) 1.00     2272
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)   1.00     3317
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)   1.00     2199
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)   1.00     3780
    #>                                                           Tail_ESS
    #> sd(Intercept)                                                 2721
    #> sd(condition2)                                                1070
    #> sd(dprime_Intercept)                                          2937
    #> sd(dprime_condition2)                                         2717
    #> sd(c_Intercept)                                               2094
    #> sd(c_condition2)                                              1981
    #> sd(metac2zero1diff_Intercept)                                 2583
    #> sd(metac2zero1diff_condition2)                                1638
    #> sd(metac2zero2diff_Intercept)                                 2715
    #> sd(metac2zero2diff_condition2)                                2418
    #> sd(metac2zero3diff_Intercept)                                 2788
    #> sd(metac2zero3diff_condition2)                                1854
    #> sd(metac2one1diff_Intercept)                                  1785
    #> sd(metac2one1diff_condition2)                                 2466
    #> sd(metac2one2diff_Intercept)                                  2709
    #> sd(metac2one2diff_condition2)                                 2181
    #> sd(metac2one3diff_Intercept)                                  2181
    #> sd(metac2one3diff_condition2)                                 2145
    #> cor(Intercept,condition2)                                     1742
    #> cor(dprime_Intercept,dprime_condition2)                       2554
    #> cor(c_Intercept,c_condition2)                                 1982
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)     2162
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)     2646
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)     2741
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition2)       2907
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition2)       2410
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition2)       2963
    #> 
    #> Regression Coefficients:
    #>                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    #> Intercept                      0.16      0.16    -0.16     0.45 1.00     2990
    #> dprime_Intercept               0.91      0.13     0.65     1.15 1.00     1979
    #> c_Intercept                   -0.33      0.12    -0.56    -0.09 1.01      812
    #> metac2zero1diff_Intercept     -1.00      0.06    -1.13    -0.88 1.00     8644
    #> metac2zero2diff_Intercept     -1.14      0.07    -1.29    -1.00 1.00    10046
    #> metac2zero3diff_Intercept     -0.89      0.07    -1.04    -0.75 1.00     7360
    #> metac2one1diff_Intercept      -0.86      0.06    -0.97    -0.75 1.00     6918
    #> metac2one2diff_Intercept      -0.97      0.06    -1.09    -0.85 1.00     8982
    #> metac2one3diff_Intercept      -1.01      0.06    -1.13    -0.88 1.00     9054
    #> condition2                    -0.20      0.17    -0.54     0.13 1.00     3924
    #> dprime_condition2              0.32      0.18    -0.03     0.69 1.00     2235
    #> c_condition2                   0.32      0.15     0.02     0.60 1.00      865
    #> metac2zero1diff_condition2     0.00      0.10    -0.19     0.19 1.00     6106
    #> metac2zero2diff_condition2     0.05      0.10    -0.15     0.25 1.00     8663
    #> metac2zero3diff_condition2    -0.09      0.10    -0.30     0.11 1.00     8202
    #> metac2one1diff_condition2     -0.22      0.09    -0.39    -0.06 1.00     7588
    #> metac2one2diff_condition2     -0.03      0.09    -0.21     0.15 1.00     6988
    #> metac2one3diff_condition2     -0.02      0.09    -0.21     0.16 1.00     7953
    #>                            Tail_ESS
    #> Intercept                      3144
    #> dprime_Intercept               2522
    #> c_Intercept                    1464
    #> metac2zero1diff_Intercept      2919
    #> metac2zero2diff_Intercept      2659
    #> metac2zero3diff_Intercept      3380
    #> metac2one1diff_Intercept       2976
    #> metac2one2diff_Intercept       3081
    #> metac2one3diff_Intercept       3034
    #> condition2                     2967
    #> dprime_condition2              2788
    #> c_condition2                   1786
    #> metac2zero1diff_condition2     3132
    #> metac2zero2diff_condition2     2715
    #> metac2zero3diff_condition2     3154
    #> metac2one1diff_condition2      2850
    #> metac2one2diff_condition2      3220
    #> metac2one3diff_condition2      2757
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


<!-- README.md is generated from README.Rmd. Please edit that file -->

# hmetad <a href="https://metacoglab.github.io/hmetad/"><img src="man/figures/logo.svg" align="right" height="138" alt="hmetad website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/metacoglab/hmetad/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/metacoglab/hmetad/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/hmetad)](https://CRAN.R-project.org/package=hmetad)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/hmetad?color=blue)](https://r-pkg.org/pkg/hmetad)
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
    #>  1     1        0        0       1          4
    #>  2     2        1        0       0          2
    #>  3     3        0        0       1          4
    #>  4     4        1        1       1          2
    #>  5     5        1        1       1          3
    #>  6     6        1        1       1          2
    #>  7     7        0        0       1          1
    #>  8     8        1        1       1          1
    #>  9     9        0        0       1          2
    #> 10    10        0        0       1          4
    #> # ℹ 990 more rows

You can fit an intercepts-only meta-d’ model using `fit_metad`:

``` r
library(hmetad)

m <- fit_metad(N ~ 1,
  data = d,
  prior = prior(normal(0, 1), class = Intercept) +
    set_prior("normal(0, 1)", class = c("dprime", "c", metac2_parameters(K = 4)))
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
    #> Intercept    -0.00      0.15    -0.31     0.27 1.00     3029     2827
    #> 
    #> Further Distributional Parameters:
    #>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> dprime              0.98      0.08     0.82     1.14 1.00     3595     2959
    #> c                  -0.01      0.04    -0.09     0.07 1.00     3734     3384
    #> metac2zero1diff     0.54      0.04     0.47     0.61 1.00     3929     3085
    #> metac2zero2diff     0.48      0.04     0.41     0.56 1.00     5441     3166
    #> metac2zero3diff     0.45      0.05     0.36     0.54 1.00     4644     2901
    #> metac2one1diff      0.48      0.04     0.42     0.56 1.00     4784     3057
    #> metac2one2diff      0.50      0.04     0.42     0.57 1.00     5043     3220
    #> metac2one3diff      0.56      0.05     0.47     0.66 1.00     5453     3423
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
    #>  2           1         1     2        0        0       1          1
    #>  3           1         1     3        1        0       0          2
    #>  4           1         1     4        1        1       1          4
    #>  5           1         1     5        1        1       1          2
    #>  6           1         1     6        1        1       1          1
    #>  7           1         1     7        1        0       0          2
    #>  8           1         1     8        0        0       1          1
    #>  9           1         1     9        0        1       0          2
    #> 10           1         1    10        1        1       1          2
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
    set_prior("normal(0, 1)", dpar = c("dprime", "c", metac2_parameters(K = 4)))
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
    #>                                                          Estimate Est.Error
    #> sd(Intercept)                                                0.83      0.31
    #> sd(condition)                                                0.55      0.22
    #> sd(dprime_Intercept)                                         1.19      0.21
    #> sd(dprime_condition)                                         0.79      0.14
    #> sd(c_Intercept)                                              1.01      0.16
    #> sd(c_condition)                                              0.69      0.11
    #> sd(metac2zero1diff_Intercept)                                0.11      0.09
    #> sd(metac2zero1diff_condition)                                0.07      0.06
    #> sd(metac2zero2diff_Intercept)                                0.12      0.10
    #> sd(metac2zero2diff_condition)                                0.08      0.07
    #> sd(metac2zero3diff_Intercept)                                0.11      0.10
    #> sd(metac2zero3diff_condition)                                0.07      0.06
    #> sd(metac2one1diff_Intercept)                                 0.11      0.09
    #> sd(metac2one1diff_condition)                                 0.07      0.06
    #> sd(metac2one2diff_Intercept)                                 0.19      0.17
    #> sd(metac2one2diff_condition)                                 0.14      0.11
    #> sd(metac2one3diff_Intercept)                                 0.14      0.12
    #> sd(metac2one3diff_condition)                                 0.09      0.08
    #> cor(Intercept,condition)                                    -0.60      0.35
    #> cor(dprime_Intercept,dprime_condition)                      -0.93      0.04
    #> cor(c_Intercept,c_condition)                                -0.93      0.03
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition)    -0.26      0.58
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition)    -0.31      0.57
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition)    -0.31      0.57
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition)      -0.33      0.58
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition)      -0.48      0.55
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition)      -0.34      0.57
    #>                                                          l-95% CI u-95% CI Rhat
    #> sd(Intercept)                                                0.31     1.50 1.01
    #> sd(condition)                                                0.12     1.03 1.02
    #> sd(dprime_Intercept)                                         0.85     1.69 1.00
    #> sd(dprime_condition)                                         0.57     1.11 1.00
    #> sd(c_Intercept)                                              0.75     1.36 1.01
    #> sd(c_condition)                                              0.51     0.93 1.00
    #> sd(metac2zero1diff_Intercept)                                0.00     0.34 1.00
    #> sd(metac2zero1diff_condition)                                0.00     0.22 1.00
    #> sd(metac2zero2diff_Intercept)                                0.00     0.39 1.00
    #> sd(metac2zero2diff_condition)                                0.00     0.25 1.00
    #> sd(metac2zero3diff_Intercept)                                0.00     0.36 1.00
    #> sd(metac2zero3diff_condition)                                0.00     0.24 1.00
    #> sd(metac2one1diff_Intercept)                                 0.00     0.35 1.00
    #> sd(metac2one1diff_condition)                                 0.00     0.23 1.00
    #> sd(metac2one2diff_Intercept)                                 0.01     0.61 1.00
    #> sd(metac2one2diff_condition)                                 0.01     0.42 1.01
    #> sd(metac2one3diff_Intercept)                                 0.01     0.45 1.00
    #> sd(metac2one3diff_condition)                                 0.00     0.29 1.00
    #> cor(Intercept,condition)                                    -0.94     0.49 1.01
    #> cor(dprime_Intercept,dprime_condition)                      -0.98    -0.83 1.00
    #> cor(c_Intercept,c_condition)                                -0.97    -0.86 1.00
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition)    -0.99     0.91 1.00
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition)    -0.99     0.90 1.00
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition)    -0.99     0.88 1.00
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition)      -0.99     0.90 1.00
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition)      -0.99     0.85 1.00
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition)      -0.99     0.87 1.00
    #>                                                          Bulk_ESS Tail_ESS
    #> sd(Intercept)                                                 742     1366
    #> sd(condition)                                                 331      373
    #> sd(dprime_Intercept)                                         1425     1823
    #> sd(dprime_condition)                                         1261     1663
    #> sd(c_Intercept)                                               871     1662
    #> sd(c_condition)                                               887     1534
    #> sd(metac2zero1diff_Intercept)                                2058     2239
    #> sd(metac2zero1diff_condition)                                1131     1850
    #> sd(metac2zero2diff_Intercept)                                1661     1765
    #> sd(metac2zero2diff_condition)                                1157     2090
    #> sd(metac2zero3diff_Intercept)                                1799     2239
    #> sd(metac2zero3diff_condition)                                1451     1928
    #> sd(metac2one1diff_Intercept)                                 1642     1784
    #> sd(metac2one1diff_condition)                                 1347     1668
    #> sd(metac2one2diff_Intercept)                                 1053     2104
    #> sd(metac2one2diff_condition)                                  663     1403
    #> sd(metac2one3diff_Intercept)                                 1644     2050
    #> sd(metac2one3diff_condition)                                 1210     1770
    #> cor(Intercept,condition)                                      410      508
    #> cor(dprime_Intercept,dprime_condition)                       1523     2268
    #> cor(c_Intercept,c_condition)                                 1008     1656
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition)     2204     2695
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition)     2280     2642
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition)     2971     3097
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition)       2232     2256
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition)        831     2144
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition)       2195     2547
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    #> Intercept                     0.07      0.31    -0.57     0.64 1.00     2467
    #> dprime_Intercept              0.82      0.27     0.27     1.37 1.00     1166
    #> c_Intercept                  -0.06      0.21    -0.49     0.36 1.01      569
    #> metac2zero1diff_Intercept    -1.08      0.13    -1.33    -0.84 1.00     6625
    #> metac2zero2diff_Intercept    -1.10      0.14    -1.36    -0.83 1.00     7301
    #> metac2zero3diff_Intercept    -0.90      0.15    -1.21    -0.62 1.00     7275
    #> metac2one1diff_Intercept     -1.11      0.13    -1.36    -0.87 1.00     7193
    #> metac2one2diff_Intercept     -0.91      0.14    -1.19    -0.64 1.00     6053
    #> metac2one3diff_Intercept     -0.67      0.15    -0.96    -0.39 1.00     9017
    #> condition                     0.07      0.20    -0.33     0.46 1.00     2220
    #> dprime_condition              0.09      0.18    -0.27     0.45 1.00     1102
    #> c_condition                   0.03      0.14    -0.26     0.33 1.01      589
    #> metac2zero1diff_condition     0.06      0.08    -0.10     0.22 1.00     6219
    #> metac2zero2diff_condition     0.09      0.09    -0.07     0.26 1.00     6987
    #> metac2zero3diff_condition    -0.05      0.10    -0.24     0.14 1.00     6517
    #> metac2one1diff_condition      0.04      0.08    -0.11     0.20 1.00     6730
    #> metac2one2diff_condition     -0.05      0.09    -0.23     0.13 1.00     5339
    #> metac2one3diff_condition     -0.18      0.09    -0.37     0.00 1.00     7544
    #>                           Tail_ESS
    #> Intercept                     2774
    #> dprime_Intercept              1747
    #> c_Intercept                   1148
    #> metac2zero1diff_Intercept     2665
    #> metac2zero2diff_Intercept     3053
    #> metac2zero3diff_Intercept     2818
    #> metac2one1diff_Intercept      2953
    #> metac2one2diff_Intercept      3015
    #> metac2one3diff_Intercept      3038
    #> condition                     2279
    #> dprime_condition              1756
    #> c_condition                   1061
    #> metac2zero1diff_condition     2862
    #> metac2zero2diff_condition     2923
    #> metac2zero3diff_condition     2620
    #> metac2one1diff_condition      2867
    #> metac2one2diff_condition      2937
    #> metac2one3diff_condition      3255
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

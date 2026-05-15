
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
    #>  1     1        0        0       1          2
    #>  2     2        1        1       1          1
    #>  3     3        0        0       1          1
    #>  4     4        0        0       1          3
    #>  5     5        0        1       0          1
    #>  6     6        1        0       0          2
    #>  7     7        1        0       0          1
    #>  8     8        0        0       1          1
    #>  9     9        0        1       0          2
    #> 10    10        0        0       1          3
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
    #> Intercept    -0.17      0.15    -0.46     0.12 1.00     3876     3016
    #> 
    #> Further Distributional Parameters:
    #>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> dprime              1.10      0.08     0.94     1.27 1.00     4747     3430
    #> c                  -0.05      0.04    -0.13     0.03 1.00     3890     3526
    #> metac2zero1diff     0.49      0.04     0.42     0.56 1.00     4760     3070
    #> metac2zero2diff     0.45      0.04     0.38     0.52 1.00     5892     3409
    #> metac2zero3diff     0.47      0.04     0.39     0.57 1.00     5573     2954
    #> metac2one1diff      0.52      0.04     0.46     0.60 1.00     4692     3480
    #> metac2one2diff      0.51      0.04     0.43     0.58 1.00     5756     3242
    #> metac2one3diff      0.47      0.05     0.38     0.57 1.00     6404     2916
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
    #>  1           1         1     1        1        1       1          4
    #>  2           1         1     2        1        1       1          4
    #>  3           1         1     3        0        0       1          2
    #>  4           1         1     4        1        1       1          4
    #>  5           1         1     5        1        1       1          4
    #>  6           1         1     6        0        0       1          1
    #>  7           1         1     7        1        1       1          3
    #>  8           1         1     8        1        1       1          4
    #>  9           1         1     9        0        0       1          3
    #> 10           1         1    10        0        1       0          3
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
    #> sd(Intercept)                                                0.98      0.33
    #> sd(condition)                                                0.63      0.19
    #> sd(dprime_Intercept)                                         0.61      0.19
    #> sd(dprime_condition)                                         0.52      0.12
    #> sd(c_Intercept)                                              1.36      0.21
    #> sd(c_condition)                                              0.92      0.14
    #> sd(metac2zero1diff_Intercept)                                0.11      0.08
    #> sd(metac2zero1diff_condition)                                0.07      0.05
    #> sd(metac2zero2diff_Intercept)                                0.07      0.06
    #> sd(metac2zero2diff_condition)                                0.04      0.04
    #> sd(metac2zero3diff_Intercept)                                0.14      0.11
    #> sd(metac2zero3diff_condition)                                0.11      0.08
    #> sd(metac2one1diff_Intercept)                                 0.13      0.10
    #> sd(metac2one1diff_condition)                                 0.08      0.06
    #> sd(metac2one2diff_Intercept)                                 0.11      0.11
    #> sd(metac2one2diff_condition)                                 0.08      0.07
    #> sd(metac2one3diff_Intercept)                                 0.09      0.08
    #> sd(metac2one3diff_condition)                                 0.06      0.05
    #> cor(Intercept,condition)                                    -0.87      0.13
    #> cor(dprime_Intercept,dprime_condition)                      -0.78      0.18
    #> cor(c_Intercept,c_condition)                                -0.94      0.03
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition)    -0.26      0.58
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition)    -0.31      0.58
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition)    -0.25      0.58
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition)      -0.24      0.58
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition)      -0.38      0.56
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition)      -0.24      0.58
    #>                                                          l-95% CI u-95% CI Rhat
    #> sd(Intercept)                                                0.40     1.71 1.00
    #> sd(condition)                                                0.31     1.05 1.00
    #> sd(dprime_Intercept)                                         0.21     0.99 1.00
    #> sd(dprime_condition)                                         0.30     0.78 1.01
    #> sd(c_Intercept)                                              1.03     1.84 1.00
    #> sd(c_condition)                                              0.69     1.24 1.00
    #> sd(metac2zero1diff_Intercept)                                0.00     0.32 1.00
    #> sd(metac2zero1diff_condition)                                0.00     0.20 1.00
    #> sd(metac2zero2diff_Intercept)                                0.00     0.24 1.00
    #> sd(metac2zero2diff_condition)                                0.00     0.16 1.00
    #> sd(metac2zero3diff_Intercept)                                0.01     0.41 1.00
    #> sd(metac2zero3diff_condition)                                0.01     0.30 1.00
    #> sd(metac2one1diff_Intercept)                                 0.01     0.37 1.00
    #> sd(metac2one1diff_condition)                                 0.00     0.23 1.00
    #> sd(metac2one2diff_Intercept)                                 0.00     0.42 1.00
    #> sd(metac2one2diff_condition)                                 0.00     0.28 1.00
    #> sd(metac2one3diff_Intercept)                                 0.00     0.29 1.00
    #> sd(metac2one3diff_condition)                                 0.00     0.20 1.00
    #> cor(Intercept,condition)                                    -0.98    -0.52 1.00
    #> cor(dprime_Intercept,dprime_condition)                      -0.95    -0.29 1.01
    #> cor(c_Intercept,c_condition)                                -0.98    -0.88 1.00
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition)    -0.98     0.90 1.00
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition)    -0.99     0.89 1.00
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition)    -0.98     0.90 1.00
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition)      -0.98     0.92 1.00
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition)      -0.99     0.87 1.00
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition)      -0.98     0.92 1.00
    #>                                                          Bulk_ESS Tail_ESS
    #> sd(Intercept)                                                 995     1215
    #> sd(condition)                                                 964      882
    #> sd(dprime_Intercept)                                          707      438
    #> sd(dprime_condition)                                          600      435
    #> sd(c_Intercept)                                               695     1034
    #> sd(c_condition)                                               777     1264
    #> sd(metac2zero1diff_Intercept)                                1456     1808
    #> sd(metac2zero1diff_condition)                                1292     2185
    #> sd(metac2zero2diff_Intercept)                                1901     1776
    #> sd(metac2zero2diff_condition)                                1573     2053
    #> sd(metac2zero3diff_Intercept)                                1751     1904
    #> sd(metac2zero3diff_condition)                                 846     1513
    #> sd(metac2one1diff_Intercept)                                 1639     2016
    #> sd(metac2one1diff_condition)                                 1082     1915
    #> sd(metac2one2diff_Intercept)                                 1263     1529
    #> sd(metac2one2diff_condition)                                 1102     1469
    #> sd(metac2one3diff_Intercept)                                 2182     2461
    #> sd(metac2one3diff_condition)                                 1391     1810
    #> cor(Intercept,condition)                                     1109     1196
    #> cor(dprime_Intercept,dprime_condition)                        503      322
    #> cor(c_Intercept,c_condition)                                  840     1574
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition)     2113     2536
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition)     2630     2606
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition)     1298     2073
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition)       1895     2288
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition)       1777     2593
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition)       2240     2535
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    #> Intercept                    -0.37      0.33    -1.05     0.22 1.00     2321
    #> dprime_Intercept              0.96      0.17     0.61     1.29 1.00     2655
    #> c_Intercept                  -0.29      0.27    -0.83     0.23 1.01      440
    #> metac2zero1diff_Intercept    -1.14      0.13    -1.41    -0.89 1.00     5817
    #> metac2zero2diff_Intercept    -1.22      0.14    -1.50    -0.94 1.00     5432
    #> metac2zero3diff_Intercept    -1.21      0.17    -1.55    -0.89 1.00     5732
    #> metac2one1diff_Intercept     -1.10      0.13    -1.37    -0.84 1.00     5740
    #> metac2one2diff_Intercept     -0.90      0.13    -1.15    -0.66 1.00     5427
    #> metac2one3diff_Intercept     -1.29      0.14    -1.58    -1.01 1.00     5348
    #> condition                     0.21      0.20    -0.17     0.62 1.00     2375
    #> dprime_condition              0.02      0.13    -0.22     0.29 1.00     1769
    #> c_condition                   0.17      0.18    -0.19     0.56 1.01      450
    #> metac2zero1diff_condition     0.07      0.09    -0.09     0.24 1.00     6184
    #> metac2zero2diff_condition     0.16      0.09    -0.01     0.33 1.00     5679
    #> metac2zero3diff_condition     0.05      0.11    -0.16     0.26 1.00     5139
    #> metac2one1diff_condition      0.04      0.09    -0.12     0.21 1.00     5380
    #> metac2one2diff_condition     -0.06      0.08    -0.22     0.10 1.00     5096
    #> metac2one3diff_condition      0.19      0.09     0.01     0.37 1.00     4889
    #>                           Tail_ESS
    #> Intercept                     2756
    #> dprime_Intercept              2936
    #> c_Intercept                    821
    #> metac2zero1diff_Intercept     2963
    #> metac2zero2diff_Intercept     2356
    #> metac2zero3diff_Intercept     2583
    #> metac2one1diff_Intercept      3103
    #> metac2one2diff_Intercept      3000
    #> metac2one3diff_Intercept      2876
    #> condition                     2830
    #> dprime_condition              2532
    #> c_condition                    886
    #> metac2zero1diff_condition     2844
    #> metac2zero2diff_condition     3018
    #> metac2zero3diff_condition     2776
    #> metac2one1diff_condition      3233
    #> metac2one2diff_condition      3120
    #> metac2one3diff_condition      2743
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


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
- Derivation of novel metacognitive bias metrics ([O’Neill et al.,
  2026](#ref-oneill2026)), and well as established metrics of
  metacognitive efficiency
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
    #>  3     3        0        0       1          3
    #>  4     4        0        0       1          2
    #>  5     5        0        0       1          2
    #>  6     6        0        1       0          1
    #>  7     7        1        1       1          2
    #>  8     8        1        1       1          1
    #>  9     9        0        1       0          1
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
    #> Intercept    -0.08      0.15    -0.39     0.20 1.00     3739     3249
    #> 
    #> Further Distributional Parameters:
    #>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> dprime              1.09      0.08     0.92     1.25 1.00     4036     3269
    #> c                   0.04      0.04    -0.04     0.13 1.00     3806     3068
    #> metac2zero1diff     0.50      0.04     0.44     0.57 1.00     4337     3379
    #> metac2zero2diff     0.51      0.04     0.43     0.59 1.00     5139     2931
    #> metac2zero3diff     0.53      0.05     0.44     0.64 1.00     5856     3372
    #> metac2one1diff      0.49      0.04     0.42     0.57 1.00     4536     2855
    #> metac2one2diff      0.46      0.04     0.39     0.54 1.00     5824     3297
    #> metac2one3diff      0.44      0.05     0.36     0.54 1.00     5905     2856
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
    #>  2           1         1     2        0        0       1          4
    #>  3           1         1     3        1        0       0          4
    #>  4           1         1     4        0        0       1          4
    #>  5           1         1     5        0        0       1          2
    #>  6           1         1     6        0        0       1          4
    #>  7           1         1     7        1        0       0          2
    #>  8           1         1     8        1        0       0          2
    #>  9           1         1     9        0        1       0          1
    #> 10           1         1    10        1        0       0          3
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
    #> sd(Intercept)                                                1.12      0.29
    #> sd(condition)                                                0.61      0.21
    #> sd(dprime_Intercept)                                         1.22      0.23
    #> sd(dprime_condition)                                         0.76      0.14
    #> sd(c_Intercept)                                              1.34      0.20
    #> sd(c_condition)                                              0.82      0.12
    #> sd(metac2zero1diff_Intercept)                                0.09      0.08
    #> sd(metac2zero1diff_condition)                                0.05      0.05
    #> sd(metac2zero2diff_Intercept)                                0.12      0.09
    #> sd(metac2zero2diff_condition)                                0.08      0.06
    #> sd(metac2zero3diff_Intercept)                                0.10      0.09
    #> sd(metac2zero3diff_condition)                                0.07      0.06
    #> sd(metac2one1diff_Intercept)                                 0.14      0.14
    #> sd(metac2one1diff_condition)                                 0.08      0.08
    #> sd(metac2one2diff_Intercept)                                 0.10      0.09
    #> sd(metac2one2diff_condition)                                 0.07      0.06
    #> sd(metac2one3diff_Intercept)                                 0.13      0.11
    #> sd(metac2one3diff_condition)                                 0.08      0.06
    #> cor(Intercept,condition)                                    -0.82      0.17
    #> cor(dprime_Intercept,dprime_condition)                      -0.95      0.03
    #> cor(c_Intercept,c_condition)                                -0.95      0.02
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition)    -0.30      0.58
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition)    -0.26      0.58
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition)    -0.27      0.57
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition)      -0.42      0.57
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition)      -0.25      0.58
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition)      -0.30      0.57
    #>                                                          l-95% CI u-95% CI Rhat
    #> sd(Intercept)                                                0.61     1.74 1.00
    #> sd(condition)                                                0.21     1.05 1.00
    #> sd(dprime_Intercept)                                         0.83     1.72 1.00
    #> sd(dprime_condition)                                         0.53     1.06 1.00
    #> sd(c_Intercept)                                              1.00     1.79 1.00
    #> sd(c_condition)                                              0.61     1.09 1.00
    #> sd(metac2zero1diff_Intercept)                                0.00     0.31 1.00
    #> sd(metac2zero1diff_condition)                                0.00     0.18 1.00
    #> sd(metac2zero2diff_Intercept)                                0.01     0.36 1.00
    #> sd(metac2zero2diff_condition)                                0.00     0.24 1.00
    #> sd(metac2zero3diff_Intercept)                                0.00     0.33 1.00
    #> sd(metac2zero3diff_condition)                                0.00     0.22 1.00
    #> sd(metac2one1diff_Intercept)                                 0.00     0.52 1.00
    #> sd(metac2one1diff_condition)                                 0.00     0.32 1.00
    #> sd(metac2one2diff_Intercept)                                 0.00     0.33 1.00
    #> sd(metac2one2diff_condition)                                 0.00     0.21 1.00
    #> sd(metac2one3diff_Intercept)                                 0.00     0.40 1.00
    #> sd(metac2one3diff_condition)                                 0.00     0.24 1.00
    #> cor(Intercept,condition)                                    -0.97    -0.35 1.00
    #> cor(dprime_Intercept,dprime_condition)                      -0.98    -0.87 1.00
    #> cor(c_Intercept,c_condition)                                -0.98    -0.89 1.00
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition)    -0.99     0.91 1.00
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition)    -0.98     0.91 1.00
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition)    -0.98     0.90 1.00
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition)      -1.00     0.87 1.00
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition)      -0.98     0.90 1.00
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition)      -0.98     0.88 1.00
    #>                                                          Bulk_ESS Tail_ESS
    #> sd(Intercept)                                                1633     1893
    #> sd(condition)                                                1015      840
    #> sd(dprime_Intercept)                                         1713     2499
    #> sd(dprime_condition)                                         1602     2405
    #> sd(c_Intercept)                                               996     1892
    #> sd(c_condition)                                              1023     1865
    #> sd(metac2zero1diff_Intercept)                                2192     2553
    #> sd(metac2zero1diff_condition)                                1918     2121
    #> sd(metac2zero2diff_Intercept)                                2491     2407
    #> sd(metac2zero2diff_condition)                                1085     2233
    #> sd(metac2zero3diff_Intercept)                                2414     2608
    #> sd(metac2zero3diff_condition)                                1673     2448
    #> sd(metac2one1diff_Intercept)                                 1071     1991
    #> sd(metac2one1diff_condition)                                  985     1970
    #> sd(metac2one2diff_Intercept)                                 2512     2216
    #> sd(metac2one2diff_condition)                                 1500     2085
    #> sd(metac2one3diff_Intercept)                                 2100     2263
    #> sd(metac2one3diff_condition)                                 1697     2283
    #> cor(Intercept,condition)                                     1687     1380
    #> cor(dprime_Intercept,dprime_condition)                       1585     2212
    #> cor(c_Intercept,c_condition)                                 1189     1869
    #> cor(metac2zero1diff_Intercept,metac2zero1diff_condition)     3365     3415
    #> cor(metac2zero2diff_Intercept,metac2zero2diff_condition)     1666     2367
    #> cor(metac2zero3diff_Intercept,metac2zero3diff_condition)     2463     3058
    #> cor(metac2one1diff_Intercept,metac2one1diff_condition)       1536     2075
    #> cor(metac2one2diff_Intercept,metac2one2diff_condition)       2629     2965
    #> cor(metac2one3diff_Intercept,metac2one3diff_condition)       2761     2718
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    #> Intercept                    -0.04      0.32    -0.69     0.58 1.00     2513
    #> dprime_Intercept              1.57      0.28     1.02     2.14 1.00     1887
    #> c_Intercept                  -0.09      0.27    -0.62     0.44 1.01      752
    #> metac2zero1diff_Intercept    -1.02      0.13    -1.27    -0.77 1.00     6385
    #> metac2zero2diff_Intercept    -0.93      0.14    -1.21    -0.66 1.00     6216
    #> metac2zero3diff_Intercept    -0.92      0.15    -1.20    -0.64 1.00     7931
    #> metac2one1diff_Intercept     -0.86      0.14    -1.13    -0.60 1.00     7051
    #> metac2one2diff_Intercept     -1.24      0.15    -1.53    -0.96 1.00     8086
    #> metac2one3diff_Intercept     -0.88      0.15    -1.18    -0.59 1.00     7211
    #> condition                     0.06      0.20    -0.34     0.43 1.00     2639
    #> dprime_condition             -0.31      0.17    -0.66     0.03 1.00     1858
    #> c_condition                   0.12      0.16    -0.21     0.43 1.00      790
    #> metac2zero1diff_condition     0.02      0.08    -0.13     0.18 1.00     6755
    #> metac2zero2diff_condition    -0.07      0.09    -0.24     0.10 1.00     6261
    #> metac2zero3diff_condition    -0.05      0.09    -0.23     0.13 1.00     7378
    #> metac2one1diff_condition     -0.09      0.09    -0.26     0.08 1.00     6693
    #> metac2one2diff_condition      0.16      0.09    -0.02     0.33 1.00     7455
    #> metac2one3diff_condition     -0.02      0.10    -0.20     0.17 1.00     7016
    #>                           Tail_ESS
    #> Intercept                     2789
    #> dprime_Intercept              2562
    #> c_Intercept                   1132
    #> metac2zero1diff_Intercept     3051
    #> metac2zero2diff_Intercept     2886
    #> metac2zero3diff_Intercept     2703
    #> metac2one1diff_Intercept      2903
    #> metac2one2diff_Intercept      2678
    #> metac2one3diff_Intercept      2987
    #> condition                     2617
    #> dprime_condition              2606
    #> c_condition                   1310
    #> metac2zero1diff_condition     3171
    #> metac2zero2diff_condition     2950
    #> metac2zero3diff_condition     2604
    #> metac2one1diff_condition      2746
    #> metac2one2diff_condition      2648
    #> metac2one3diff_condition      3011
    #> 
    #> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    #> and Tail_ESS are effective sample size measures, and Rhat is the potential
    #> scale reduction factor on split chains (at convergence, Rhat = 1).

## References

<div id="refs" class="references csl-bib-body hanging-indent"
data-entry-spacing="0" data-line-spacing="2">

<div id="ref-fleming2017" class="csl-entry">

Fleming, S. M. (2017). HMeta-d: Hierarchical bayesian estimation of
metacognitive efficiency from confidence ratings. *Neuroscience of
Consciousness*, *2017*(1). <https://doi.org/10.1093/nc/nix007>

</div>

<div id="ref-maniscalco2012" class="csl-entry">

Maniscalco, B., & Lau, H. (2012). A signal detection theoretic approach
for estimating metacognitive sensitivity from confidence ratings.
*Consciousness and Cognition*, *21*(1), 422–430.
<https://doi.org/10.1016/j.concog.2011.09.021>

</div>

<div id="ref-maniscalco2014" class="csl-entry">

Maniscalco, B., & Lau, H. (2014). Signal detection theory analysis of
type 1 and type 2 data: Meta-d′, response-specific meta-d′, and the
unequal variance SDT model. In *The cognitive neuroscience of
metacognition* (pp. 25–66). Springer Berlin Heidelberg.
<https://doi.org/10.1007/978-3-642-45190-4_3>

</div>

<div id="ref-oneill2026" class="csl-entry">

O’Neill, K., Zhan, T., & Fleming, S. M. (2026). A measure of
metacognitive bias you can be confident in. *Proceedings of the Annual
Meeting of the Cognitive Science Society*, *48*.
<https://escholarship.org/uc/item/40m9h05x>

</div>

</div>

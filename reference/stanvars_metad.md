# Generate Stan code for the meta-d' model

Generate Stan code for the meta-d' model

## Usage

``` r
stanvars_metad(
  K,
  distribution = "normal",
  metac_absolute = TRUE,
  categorical = FALSE
)
```

## Arguments

- K:

  The number of confidence levels

- distribution:

  The noise distribution to use. Should be a parameter-free
  distribution, i.e., one that is mean-centered without additional
  variance/shape parameters. If the distribution is not already
  available in stan, you must additionally provide two functions to Stan
  (one for `<distribution>_lcdf` and one for `<distribution>_lccdf`).

- metac_absolute:

  Should the type 2 criterion (metac) be fixed to the absolute type 1
  criterion (c)? If `TRUE`, the model will set `metac = c`. Otherwise,
  it will set `metac = M * c`, such that the type 2 criterion is
  *relatively* equal to the type 1 criterion (i.e.,
  `meta_c/meta_dprime = c/dprime`)

- categorical:

  If `FALSE` (default), use the multinomial likelihood over aggregated
  data. If `TRUE`, use the categorical likelihood over individual
  trials.

## Value

A [brms::stanvar](https://paulbuerkner.com/brms/reference/stanvar.html)
object containing Stan code defining the likelihood for the metad' model
with `K` confidence levels, signal distributed according to the
distribution `distribution`, and where `metac = c` if
`metac_absolute==TRUE`, and `metac = M*c` otherwise.

## Examples

``` r
# create stancode for the meta-d' model
# using the normal distribution and 3 levels of confidence
stanvars_metad(3)
#> [[1]]
#> [[1]]$name
#> [1] ""
#> 
#> [[1]]$sdata
#> NULL
#> 
#> [[1]]$scode
#> [1] "\n  // Convert a binary int x from {0, 1} to {-1, 1}\n  int to_signed(int x) {\n    return 2*x - 1;\n  }\n\n  // P(response, confidence | stimulus) given as simplex\n  // [P(resp=0, conf=K), .... P(resp=0, conf=1), P(resp=1, conf=1), ... P(resp=1, conf=K)]\n  vector metad_normal_pmf(int stimulus, real dprime, real c, real meta_dprime, real meta_c, vector meta_c2_0, vector meta_c2_1) {\n    // number of confidence levels\n    int K = size(meta_c2_0) + 1;\n\n    // type-1 response probabilities\n    real lp_1 = std_normal_lccdf(c - to_signed(stimulus)*dprime/2);\n    real lp_0 = std_normal_lcdf(c - to_signed(stimulus)*dprime/2);\n\n    // means of type-2 distributions\n    real meta_mu = to_signed(stimulus) * meta_dprime/2;\n\n    vector[K] lp2_1;         // CDFs (response == 1)\n    vector[K] lp2_0;         // CDFs (response == 0)\n    vector[2*K] log_theta;   // joint (type-1 x type-2) response probabilities\n\n    lp2_1[1] = std_normal_lccdf(meta_c - meta_mu);\n    lp2_0[1] = std_normal_lcdf(meta_c - meta_mu);\n    for (k in 2:K) {\n      lp2_1[k] = std_normal_lccdf(meta_c2_1[k-1] - meta_mu);\n      lp2_0[k] = std_normal_lcdf(meta_c2_0[k-1] - meta_mu);\n\n      log_theta[K-k+2] = log_diff_exp(lp2_0[k-1], lp2_0[k]);\n      log_theta[K+k-1] = log_diff_exp(lp2_1[k-1], lp2_1[k]);\n    }\n    log_theta[1] = lp2_0[K];\n    log_theta[2*K] = lp2_1[K];\n\n    // weight by P(response|stimulus) and normalize\n    log_theta[1:K] += lp_0 - lp2_0[1];\n    log_theta[(K+1):(2*K)] += lp_1 - lp2_1[1];\n\n    return exp(log_theta);\n  }\n\n  real metad__3__normal__absolute__multinomial_lpmf(array[] int Y, real M, real dprime, real c, real z_meta_c2_0_1, real z_meta_c2_0_2, real z_meta_c2_1_1, real z_meta_c2_1_2) {\n  int K = 3; // number of confidence levels\n\n  real meta_dprime = M * dprime;\n  real meta_c = c;\n  vector[K-1] meta_c2_0 = meta_c - cumulative_sum([z_meta_c2_0_1, z_meta_c2_0_2]');\n  vector[K-1] meta_c2_1 = meta_c + cumulative_sum([z_meta_c2_1_1, z_meta_c2_1_2]');\n\n  // use multinomial likelihood\n  return multinomial_lpmf(Y[1:(2*K)] | metad_normal_pmf(0, dprime, c,\n                          meta_dprime, meta_c, meta_c2_0, meta_c2_1)) +\n    multinomial_lpmf(Y[(2*K+1):(4*K)] |  metad_normal_pmf(1, dprime, c,\n                      meta_dprime, meta_c, meta_c2_0, meta_c2_1));\n}"
#> 
#> [[1]]$block
#> [1] "functions"
#> 
#> [[1]]$position
#> [1] "start"
#> 
#> [[1]]$pll_args
#> character(0)
#> 
#> 
#> attr(,"class")
#> [1] "stanvars"

# create stancode for the meta-d' model with meta_c = M * c
stanvars_metad(3, metac_absolute = FALSE)
#> [[1]]
#> [[1]]$name
#> [1] ""
#> 
#> [[1]]$sdata
#> NULL
#> 
#> [[1]]$scode
#> [1] "\n  // Convert a binary int x from {0, 1} to {-1, 1}\n  int to_signed(int x) {\n    return 2*x - 1;\n  }\n\n  // P(response, confidence | stimulus) given as simplex\n  // [P(resp=0, conf=K), .... P(resp=0, conf=1), P(resp=1, conf=1), ... P(resp=1, conf=K)]\n  vector metad_normal_pmf(int stimulus, real dprime, real c, real meta_dprime, real meta_c, vector meta_c2_0, vector meta_c2_1) {\n    // number of confidence levels\n    int K = size(meta_c2_0) + 1;\n\n    // type-1 response probabilities\n    real lp_1 = std_normal_lccdf(c - to_signed(stimulus)*dprime/2);\n    real lp_0 = std_normal_lcdf(c - to_signed(stimulus)*dprime/2);\n\n    // means of type-2 distributions\n    real meta_mu = to_signed(stimulus) * meta_dprime/2;\n\n    vector[K] lp2_1;         // CDFs (response == 1)\n    vector[K] lp2_0;         // CDFs (response == 0)\n    vector[2*K] log_theta;   // joint (type-1 x type-2) response probabilities\n\n    lp2_1[1] = std_normal_lccdf(meta_c - meta_mu);\n    lp2_0[1] = std_normal_lcdf(meta_c - meta_mu);\n    for (k in 2:K) {\n      lp2_1[k] = std_normal_lccdf(meta_c2_1[k-1] - meta_mu);\n      lp2_0[k] = std_normal_lcdf(meta_c2_0[k-1] - meta_mu);\n\n      log_theta[K-k+2] = log_diff_exp(lp2_0[k-1], lp2_0[k]);\n      log_theta[K+k-1] = log_diff_exp(lp2_1[k-1], lp2_1[k]);\n    }\n    log_theta[1] = lp2_0[K];\n    log_theta[2*K] = lp2_1[K];\n\n    // weight by P(response|stimulus) and normalize\n    log_theta[1:K] += lp_0 - lp2_0[1];\n    log_theta[(K+1):(2*K)] += lp_1 - lp2_1[1];\n\n    return exp(log_theta);\n  }\n\n  real metad__3__normal__relative__multinomial_lpmf(array[] int Y, real M, real dprime, real c, real z_meta_c2_0_1, real z_meta_c2_0_2, real z_meta_c2_1_1, real z_meta_c2_1_2) {\n  int K = 3; // number of confidence levels\n\n  real meta_dprime = M * dprime;\n  real meta_c = M * c;\n  vector[K-1] meta_c2_0 = meta_c - cumulative_sum([z_meta_c2_0_1, z_meta_c2_0_2]');\n  vector[K-1] meta_c2_1 = meta_c + cumulative_sum([z_meta_c2_1_1, z_meta_c2_1_2]');\n\n  // use multinomial likelihood\n  return multinomial_lpmf(Y[1:(2*K)] | metad_normal_pmf(0, dprime, c,\n                          meta_dprime, meta_c, meta_c2_0, meta_c2_1)) +\n    multinomial_lpmf(Y[(2*K+1):(4*K)] |  metad_normal_pmf(1, dprime, c,\n                      meta_dprime, meta_c, meta_c2_0, meta_c2_1));\n}"
#> 
#> [[1]]$block
#> [1] "functions"
#> 
#> [[1]]$position
#> [1] "start"
#> 
#> [[1]]$pll_args
#> character(0)
#> 
#> 
#> attr(,"class")
#> [1] "stanvars"

# create stancode for the meta-d' model with
# an alternative distribution
# note: cumulative distribution functions must be defined
# in R and in Stan using [brms::stanvar()]
stanvars_metad(4, distribution = "gumbel_min")
#> [[1]]
#> [[1]]$name
#> [1] ""
#> 
#> [[1]]$sdata
#> NULL
#> 
#> [[1]]$scode
#> [1] "\n  // Convert a binary int x from {0, 1} to {-1, 1}\n  int to_signed(int x) {\n    return 2*x - 1;\n  }\n\n  // P(response, confidence | stimulus) given as simplex\n  // [P(resp=0, conf=K), .... P(resp=0, conf=1), P(resp=1, conf=1), ... P(resp=1, conf=K)]\n  vector metad_gumbel_min_pmf(int stimulus, real dprime, real c, real meta_dprime, real meta_c, vector meta_c2_0, vector meta_c2_1) {\n    // number of confidence levels\n    int K = size(meta_c2_0) + 1;\n\n    // type-1 response probabilities\n    real lp_1 = gumbel_min_lccdf(c | to_signed(stimulus)*dprime/2);\n    real lp_0 = gumbel_min_lcdf(c | to_signed(stimulus)*dprime/2);\n\n    // means of type-2 distributions\n    real meta_mu = to_signed(stimulus) * meta_dprime/2;\n\n    vector[K] lp2_1;         // CDFs (response == 1)\n    vector[K] lp2_0;         // CDFs (response == 0)\n    vector[2*K] log_theta;   // joint (type-1 x type-2) response probabilities\n\n    lp2_1[1] = gumbel_min_lccdf(meta_c | meta_mu);\n    lp2_0[1] = gumbel_min_lcdf(meta_c | meta_mu);\n    for (k in 2:K) {\n      lp2_1[k] = gumbel_min_lccdf(meta_c2_1[k-1] | meta_mu);\n      lp2_0[k] = gumbel_min_lcdf(meta_c2_0[k-1] | meta_mu);\n\n      log_theta[K-k+2] = log_diff_exp(lp2_0[k-1], lp2_0[k]);\n      log_theta[K+k-1] = log_diff_exp(lp2_1[k-1], lp2_1[k]);\n    }\n    log_theta[1] = lp2_0[K];\n    log_theta[2*K] = lp2_1[K];\n\n    // weight by P(response|stimulus) and normalize\n    log_theta[1:K] += lp_0 - lp2_0[1];\n    log_theta[(K+1):(2*K)] += lp_1 - lp2_1[1];\n\n    return exp(log_theta);\n  }\n\n  real metad__4__gumbel_min__absolute__multinomial_lpmf(array[] int Y, real M, real dprime, real c, real z_meta_c2_0_1, real z_meta_c2_0_2, real z_meta_c2_0_3, real z_meta_c2_1_1, real z_meta_c2_1_2, real z_meta_c2_1_3) {\n  int K = 4; // number of confidence levels\n\n  real meta_dprime = M * dprime;\n  real meta_c = c;\n  vector[K-1] meta_c2_0 = meta_c - cumulative_sum([z_meta_c2_0_1, z_meta_c2_0_2, z_meta_c2_0_3]');\n  vector[K-1] meta_c2_1 = meta_c + cumulative_sum([z_meta_c2_1_1, z_meta_c2_1_2, z_meta_c2_1_3]');\n\n  // use multinomial likelihood\n  return multinomial_lpmf(Y[1:(2*K)] | metad_gumbel_min_pmf(0, dprime, c,\n                          meta_dprime, meta_c, meta_c2_0, meta_c2_1)) +\n    multinomial_lpmf(Y[(2*K+1):(4*K)] |  metad_gumbel_min_pmf(1, dprime, c,\n                      meta_dprime, meta_c, meta_c2_0, meta_c2_1));\n}"
#> 
#> [[1]]$block
#> [1] "functions"
#> 
#> [[1]]$position
#> [1] "start"
#> 
#> [[1]]$pll_args
#> character(0)
#> 
#> 
#> attr(,"class")
#> [1] "stanvars"
```

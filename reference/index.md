# Package index

## Fitting the meta-d’ model

Run the meta-d’ model on aggregated or non-aggregated data using `brms`

- [`fit_metad()`](https://metacoglab.github.io/mRatio/reference/fit_metad.md)
  :

  Fit the meta-d' model using `brms` package

- [`metad()`](https://metacoglab.github.io/mRatio/reference/metad.md) :

  `brms` family for the metad' model

- [`stanvars_metad()`](https://metacoglab.github.io/mRatio/reference/stanvars_metad.md)
  : Generate Stan code for the meta-d' model

## Processing data for model fitting

Transform data between common formats used in metacognition research

- [`to_signed()`](https://metacoglab.github.io/mRatio/reference/signed.md)
  [`to_unsigned()`](https://metacoglab.github.io/mRatio/reference/signed.md)
  : Convert binary variable \\x\\ between \\\\0, 1\\\\ and \\\\-1, 1\\\\

- [`joint_response()`](https://metacoglab.github.io/mRatio/reference/responses.md)
  [`type1_response()`](https://metacoglab.github.io/mRatio/reference/responses.md)
  [`type2_response()`](https://metacoglab.github.io/mRatio/reference/responses.md)
  : Convert between separate and joint type 1/type 2 responses

- [`response_probabilities()`](https://metacoglab.github.io/mRatio/reference/response_probabilities.md)
  : Compute joint response probabilities from aggregated counts

- [`aggregate_metad()`](https://metacoglab.github.io/mRatio/reference/aggregate_metad.md)
  :

  Aggregate `data` by `response`, `confidence`, and other columns

## Extracting model estimates

Obtain model parameters, posterior expectations, and implied quantities
from fitted models.

- [`linpred_draws_metad()`](https://metacoglab.github.io/mRatio/reference/linpred_draws_metad.md)
  [`add_linpred_draws_metad()`](https://metacoglab.github.io/mRatio/reference/linpred_draws_metad.md)
  [`linpred_rvars_metad()`](https://metacoglab.github.io/mRatio/reference/linpred_draws_metad.md)
  [`add_linpred_rvars_metad()`](https://metacoglab.github.io/mRatio/reference/linpred_draws_metad.md)
  : Obtain posterior draws of meta-d' model parameters
- [`epred_draws_metad()`](https://metacoglab.github.io/mRatio/reference/epred_draws_metad.md)
  [`add_epred_draws_metad()`](https://metacoglab.github.io/mRatio/reference/epred_draws_metad.md)
  [`epred_rvars_metad()`](https://metacoglab.github.io/mRatio/reference/epred_draws_metad.md)
  [`add_epred_rvars_metad()`](https://metacoglab.github.io/mRatio/reference/epred_draws_metad.md)
  : Obtain posterior draws of joint response probabilities
- [`predicted_draws_metad()`](https://metacoglab.github.io/mRatio/reference/predicted_draws_metad.md)
  [`add_predicted_draws_metad()`](https://metacoglab.github.io/mRatio/reference/predicted_draws_metad.md)
  [`predicted_rvars_metad()`](https://metacoglab.github.io/mRatio/reference/predicted_draws_metad.md)
  [`add_predicted_rvars_metad()`](https://metacoglab.github.io/mRatio/reference/predicted_draws_metad.md)
  : Obtain posterior predictions of joint responses
- [`mean_confidence_draws()`](https://metacoglab.github.io/mRatio/reference/mean_conf_draws.md)
  [`add_mean_confidence_draws()`](https://metacoglab.github.io/mRatio/reference/mean_conf_draws.md)
  [`mean_confidence_rvars()`](https://metacoglab.github.io/mRatio/reference/mean_conf_draws.md)
  [`add_mean_confidence_rvars()`](https://metacoglab.github.io/mRatio/reference/mean_conf_draws.md)
  : Obtain posterior draws of mean confidence
- [`metacognitive_bias_draws()`](https://metacoglab.github.io/mRatio/reference/bias_draws.md)
  [`add_metacognitive_bias_draws()`](https://metacoglab.github.io/mRatio/reference/bias_draws.md)
  [`metacognitive_bias_rvars()`](https://metacoglab.github.io/mRatio/reference/bias_draws.md)
  [`add_metacognitive_bias_rvars()`](https://metacoglab.github.io/mRatio/reference/bias_draws.md)
  : Obtain posterior draws of an index of metacognitive bias
- [`roc1_draws()`](https://metacoglab.github.io/mRatio/reference/roc1_draws.md)
  [`add_roc1_draws()`](https://metacoglab.github.io/mRatio/reference/roc1_draws.md)
  [`roc1_rvars()`](https://metacoglab.github.io/mRatio/reference/roc1_draws.md)
  [`add_roc1_rvars()`](https://metacoglab.github.io/mRatio/reference/roc1_draws.md)
  : Obtain posterior draws of the pseudo type 1 receiver operating
  characteristic (ROC) curve.
- [`roc2_draws()`](https://metacoglab.github.io/mRatio/reference/roc2_draws.md)
  [`add_roc2_draws()`](https://metacoglab.github.io/mRatio/reference/roc2_draws.md)
  [`roc2_rvars()`](https://metacoglab.github.io/mRatio/reference/roc2_draws.md)
  [`add_roc2_rvars()`](https://metacoglab.github.io/mRatio/reference/roc2_draws.md)
  : Obtain posterior draws of the response-specific type 2 receiver
  operating characteristic (ROC) curves.

## Simulating from the meta-d’ model

Generate simulated type 1 responses and confidence ratings from the
meta-d’ model

- [`sim_metad()`](https://metacoglab.github.io/mRatio/reference/sim_metad.md)
  : Simulate from the meta-d' model
- [`sim_metad_condition()`](https://metacoglab.github.io/mRatio/reference/sim_metad_condition.md)
  : Simulate from the meta-d' model across separate conditions
- [`sim_metad_participant()`](https://metacoglab.github.io/mRatio/reference/sim_metad_participant.md)
  : Simulate from the hierarchical meta-d' model
- [`sim_metad_participant_condition()`](https://metacoglab.github.io/mRatio/reference/sim_metad_participant_condition.md)
  : Simulate from the hierarchical meta-d' model across
  within-participant conditions

## Working with common distributions

Calculate model log likelihood, response probabilities, or sample from
distributions

- [`normal_lcdf()`](https://metacoglab.github.io/mRatio/reference/normal_dist.md)
  [`normal_lccdf()`](https://metacoglab.github.io/mRatio/reference/normal_dist.md)
  : Normal cumulative distribution functions

- [`metad_pmf()`](https://metacoglab.github.io/mRatio/reference/metad_pmf.md)
  : Generate (log) probability simplex over the joint type 1/type 2
  responses

- [`cov_matrix()`](https://metacoglab.github.io/mRatio/reference/cov_matrix.md)
  : Generate a covariance matrix.

- [`cor_matrix()`](https://metacoglab.github.io/mRatio/reference/cor_matrix.md)
  :

  Generate a correlation matrix with all off-diagonal values equal to
  `r`

- [`rmatrixnorm()`](https://metacoglab.github.io/mRatio/reference/rmatrixnorm.md)
  : Sample from a matrix-normal distribution

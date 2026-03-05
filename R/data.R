#' Simulated data for example model fitting
#'
#' @description A simulated data set of 1000 trials from a two-alternative
#'   forced choice task with 4 levels of confidence.
#'
#' @format A tibble of 1000 observations containing the following columns:
#'   * `trial`: the trial number
#'   * `stimulus`: the stimulus presence (`0` or `1`)
#'   * `response`: the simulated type 1 response
#'   * `confidence`: the simulated type 2 response
#'   * `correct`: the accuracy of the simulated type 1 response
#'   * `dprime`, `c`, `meta_dprime`, `M`, `meta_c2_0`, `meta_c2_1`: the
#'   parameters of the model used for simulation
#'   * `theta`, `theta_1`, `theta_2`: the joint, type 1, and type 2 response
#'   probabilities of the model used for simulation
#' @seealso [sim_metad()]
#' @source Generated using the code `sim_metad(N_trials = 1000)`
#' @examples
#' \donttest{
#' fit_metad(N ~ 1, example_data, chains = 1, iter = 500)
#' }
"example_data"


#' Example meta-d' model for model post-processing
#'
#' @description A model fit to the simulated data [hmetad::example_data]. This
#'   model includes one constant set of parameters, with no multilevel
#'   structure.
#'
#' @format A `brmsfit` object
#' @seealso [fit_metad()]
#' @source Generated using the code `fit_metad(N ~ 1, example_data, iter = 500)`
#' @examples
#'
#' # inspect summary of posterior distribution
#' summary(example_model)
#'
#' # obtain posterior expectations
#' epred_draws_metad(example_model, tidyr::tibble(.row = 1))
#'
"example_model"

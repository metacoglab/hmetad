#' An environment to contain the example data and example model if needed by the user
#' @keywords internal
#' @noRd
the <- new.env(parent = emptyenv())
the$example_data <- NULL
the$example_model <- NULL


#' Simulated data for example model fitting
#'
#' @description A simulated data set of 1000 trials from a two-alternative
#'   forced choice task with 4 levels of confidence.
#' @returns A tibble of 1000 observations containing the following columns:
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
#' @examples
#' # Fit an empty model on the example data
#' # (remove empty=TRUE to actually fit the model)
#' fit_metad(N ~ 1, example_data(), empty = TRUE)
#' @export
example_data <- function() {
  if (is.null(the$example_data)) {
    the$example_data <- sim_metad(N_trials = 1000)
  }

  the$example_data
}

#' Example meta-d' model for model post-processing
#'
#' @description A model fit to the simulated data [hmetad::example_data]. This
#'   model includes one constant set of parameters, with no multilevel
#'   structure.
#' @returns A `brmsfit` object fitted to simulated data
#' @seealso [fit_metad()]
#' @examples
#' \donttest{
#' # inspect summary of posterior distribution
#' summary(example_model())
#'
#' # obtain posterior expectations
#' epred_draws_metad(example_model(), tidyr::tibble(.row = 1))
#' }
#' @export
example_model <- function() {
  if (is.null(the$example_model)) {
    the$example_model <- fit_metad(N ~ 1, example_data(), iter = 500)
  }

  the$example_model
}

#' Calculate empirical type 1 response probabilities
#'
#' Given a dataset `data`, determine the probability of each type 1 response,
#' optionally conditional on stimulus.
#'
#' @param data The data frame to aggregate
#' @param ... Grouping columns in `data`. These columns will be converted to
#'   factors.
#' @param .stimulus The name of "stimulus" column
#' @param .response The name of "response" column
#' @param .confidence The name of "confidence" column
#' @param .joint_response The name of "joint_response" column
#' @param K The number of confidence levels in `data`. If `NULL`, this is
#'   estimated from `data` using the maximum value of either the confidence
#'   column or joint response column.
#' @param by_stimulus If `TRUE` (default), calculate conditional type 1 response
#'   probabilities \eqn{P(R=r \vert S=s)}. Otherwise, calculate unconditional
#'   response probabilities \eqn{P(R=r)} as an unweighted average over stimuli.
#' @returns A tibble with columns:
#'  * `...`: the grouping columns in `data`
#'  * `{.stimulus}` (if `by_stimulus=TRUE`): the stimulus
#'  * `{.response}`: the type 1 response
#'  * `n`: the number of rows in `data` with the corresponding `stimulus` (if `by_stimulus=TRUE`) and `response`
#'  * `p`: the proportion of rows in `data` with the corresponding `response` (per `stimulus` if `by_stimulus=TRUE`)
#' @seealso [type1_draws()]
#' @examples
#' # calculate response probabilities by stimulus
#' type1_probabilities(example_data())
#'
#' # calculate response probabilities by condition, averaging over stimuli
#' type1_probabilities(sim_metad_condition(), condition, by_stimulus = FALSE)
#' @export
type1_probabilities <- function(
  data, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", .joint_response = "joint_response",
  K = NULL, by_stimulus = TRUE
) {
  data <- .aggregate_metad(
    data, ...,
    .stimulus = .stimulus, .response = .response,
    .confidence = .confidence, .joint_response = .joint_response, K = K
  ) |>
    group_by(..., !!sym(.stimulus), !!sym(.response))

  if (!by_stimulus) {
    data <- data |>
      group_by(..., !!sym(.response))
  }

  data <- data |>
    summarize(n = sum(.data$n)) |>
    group_by(...)

  if (by_stimulus) {
    data <- data |>
      group_by(!!sym(.stimulus), .add = TRUE)
  }

  data |>
    mutate(p = .data$n / sum(.data$n))
}


#' Calculate posterior draws of type 1 response probabilities
#'
#' @description Given a data frame and a meta-d' model, adds estimates of type 1
#'   response probabilities (i.e., \eqn{P(R=r \vert S=s)} or \eqn{P(R=r)} for
#'   type 1 response \eqn{R} and stimulus \eqn{S}). For `type1_draws_metad` and
#'   `add_type1_draws_metad`, estimates are returned in a tidy tibble with one
#'   row per posterior draw. For `type1_rvars_metad` and
#'   `add_type1_rvars_metad`, parameters are returned as [posterior::rvar]s,
#'   with one row per row in `newdata`.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional arguments passed to [tidybayes::add_epred_draws] or
#'   [tidybayes::add_epred_rvars]
#' @param .stimulus The name of "stimulus" column
#' @param .response The name of "response" column
#' @param .confidence The name of "confidence" column
#' @param .joint_response The name of "joint_response" column
#' @param by_stimulus If `TRUE` (default), calculate conditional type 1 response
#'   probabilities \eqn{P(R=r \vert S=s)}. Otherwise, calculate unconditional
#'   response probabilities \eqn{P(R=r)} as an unweighted average over stimuli.
#' @returns a tibble containing posterior draws of model parameters with the
#'   following columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: for `epred_draws_metad`, identifiers for the posterior sample
#'  * `{.stimulus}`, `{.response}`: identifiers for the response type
#'  * `.epred`: probability of the type 1 response (optionally given the stimulus)
#' @examples
#' \donttest{
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # obtain model predictions
#' # equivalent to `add_type1_draws(newdata, example_model())`
#' type1_draws(example_model(), newdata)
#'
#' # obtain model predictions (`posterior::rvar`)
#' # equivalent to `add_type1_rvars(newdata, example_model(), by_stimulus = FALSE)`
#' type1_rvars(example_model(), newdata, by_stimulus = FALSE)
#' }
#' @seealso [type1_probabilities()]
#' @rdname type1_draws
#' @export
type1_draws <- function(
  object, newdata, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", .joint_response = "joint_response",
  by_stimulus = TRUE
) {
  ## grouping columns
  .stimulus <- get_stimulus(object, .default = .stimulus)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, .joint_response, .response, .confidence
  ))]

  draws <- epred_draws_metad(
    object, newdata, ...,
    .stimulus = .stimulus, .response = .response,
    .confidence = .confidence, .joint_response = .joint_response
  ) |>
    group_by(
      .data$.row, !!!syms(.cols),
      !!sym(.stimulus), !!sym(.response), .data$.draw
    ) |>
    summarize(.epred = sum(.data$.epred)) |>
    group_by(.data$.row, !!!syms(.cols), !!sym(.stimulus), !!sym(.response))

  if (!by_stimulus) {
    draws <- draws |>
      group_by(.data$.row, !!!syms(.cols), !!sym(.response), .data$.draw) |>
      summarize(.epred = mean(.data$.epred)) |>
      group_by(.data$.row, !!!syms(.cols), !!sym(.response))
  }

  draws
}

#' @rdname type1_draws
#' @export
add_type1_draws <- function(newdata, object, ...) {
  type1_draws(object, newdata, ...)
}

#' @rdname type1_draws
#' @export
type1_rvars <- function(
  object, newdata, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", .joint_response = "joint_response",
  by_stimulus = TRUE
) {
  ## grouping columns
  .stimulus <- get_stimulus(object, .default = .stimulus)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, .joint_response, .response, .confidence
  ))]

  draws <- epred_rvars_metad(
    object, newdata, ...,
    .stimulus = .stimulus, .response = .response,
    .confidence = .confidence, .joint_response = .joint_response
  ) |>
    group_by(.data$.row, !!!syms(.cols), !!sym(.stimulus), !!sym(.response)) |>
    summarize(.epred = rvar_sum(.data$.epred), .groups = "keep")

  if (!by_stimulus) {
    draws <- draws |>
      group_by(.data$.row, !!!syms(.cols), !!sym(.response)) |>
      summarize(.epred = rvar_mean(.data$.epred), .groups = "keep")
  }

  draws
}

#' @rdname type1_draws
#' @export
add_type1_rvars <- function(newdata, object, ...) {
  type1_rvars(object, newdata, ...)
}

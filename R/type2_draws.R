#' Calculate empirical type 2 response probabilities
#'
#' Given a dataset `data`, determine the probability of each type 2 response,
#' optionally conditional on stimulus and/or type 1 response.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Grouping columns in `data`. These columns will be converted to
#'   factors.
#' @param .stimulus The name of "stimulus" column
#' @param .response The name of "response" column
#' @param .confidence The name of "confidence" column
#' @param .joint_response The name of "joint_response" column
#' @param K The number of confidence levels in `data`. If `NULL`, this is
#'   estimated from `data` using the maximum value of either the confidence
#'   column or joint response column.
#' @param by_stimulus If `TRUE` (default), calculate type 2 response
#'   probabilities conditional on stimulus.
#' @param by_response If `TRUE` (default), calculate type 2 response
#'   probabilities conditional on type 1 response.
#' @returns A tibble with columns:
#'  * `...`: the grouping columns in `data`
#'  * `{.stimulus}` (if `by_stimulus=TRUE`): the stimulus
#'  * `{.response}` (if `by_response=TRUE`): the type 1 response
#'  * `{.confidence}`: the type 2 response
#'  * `{.joint_response}` (if `by_response=TRUE`): the joint type 1/type 2 response
#'  * `n`: the number of rows in `data` with the corresponding `stimulus` (if `by_stimulus=TRUE`), `response` (if `by_response=TRUE`), and `confidence`
#'  * `p`: the proportion of rows in `data` with the corresponding `response` (per `stimulus` if `by_stimulus=TRUE` and per `response` if `by_response=TRUE`)
#' @examples
#' # calculate type 2 response probabilities by stimulus
#' type2_probabilities(example_data())
#'
#' # calculate type 2 response probabilities by condition, averaging over stimuli
#' type2_probabilities(sim_metad_condition(), condition, by_stimulus = FALSE)
#' @seealso [type2_draws()]
#' @export
type2_probabilities <- function(
  data, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", .joint_response = "joint_response",
  K = NULL, by_stimulus = TRUE, by_response = TRUE
) {
  # infer number of confidence levels
  if (is.null(K)) {
    K <- infer_confidence_levels(
      data,
      aggregate = TRUE, categorical = FALSE,
      .stimulus = .stimulus, .response = .response,
      .confidence = .confidence, .joint_response = .joint_response
    )
  }

  data <- .aggregate_metad(
    data, ...,
    .stimulus = .stimulus, .response = .response,
    .confidence = .confidence, .joint_response = .joint_response, K = K
  ) |>
    group_by(...)

  ## add grouping columns
  if (by_stimulus) {
    data <- data |>
      group_by(!!sym(.stimulus), .add = TRUE)
  }
  if (by_response) {
    data <- data |>
      group_by(!!sym(.response), .add = TRUE)
  }

  ## calculate type 2 response probabilities
  data <- data |>
    group_by(!!sym(.confidence), .add = TRUE) |>
    summarize(n = sum(.data$n), .groups = "drop_last") |>
    mutate(p = .data$n / sum(.data$n)) |>
    group_by(...)

  if (by_response) {
    data <- data |>
      mutate("{.joint_response}" := joint_response(!!sym(.response), !!sym(.confidence), K)) |>
      relocate(!!sym(.joint_response), .before = "n") |>
      arrange(!!sym(.response), !!sym(.confidence))
  }

  if (by_stimulus) {
    data <- data |>
      arrange(..., !!sym(.stimulus)) |>
      group_by(!!sym(.stimulus), .add = TRUE)
  }

  data
}


#' Calculate posterior draws of type 2 response probabilities
#'
#' @description Given a data frame and a meta-d' model, adds estimates of type 2
#'   response probabilities (i.e., \eqn{P(C=c \vert S=s, R=r)}, \eqn{P(C=c \vert
#'   S=s)}, \eqn{P(C=c \vert R=r)} or \eqn{P(C=c)} for stimulus \eqn{S}),
#'   type 1 response \eqn{R}, and type 2 response \eqn{C}. For
#'   `type2_draws_metad` and `add_type2_draws_metad`, estimates are returned in
#'   a tidy tibble with one row per posterior draw. For `type2_rvars_metad` and
#'   `add_type2_rvars_metad`, parameters are returned as [posterior::rvar]s,
#'   with one row per row in `newdata`.
#'
#' @param ... Additional arguments passed to [tidybayes::add_epred_draws] or
#'   [tidybayes::add_epred_rvars]
#' @param .stimulus The name of "stimulus" column
#' @param .response The name of "response" column
#' @param .confidence The name of "confidence" column
#' @param by_stimulus If `TRUE` (default), calculate type 2 response
#'   probabilities separately by stimulus. Otherwise, calculate unconditional
#'   type 2 response probabilities as an unweighted average over stimuli.
#' @param by_response If `TRUE` (default), calculate type 2 response
#'   probabilities separately by type 1 response. Otherwise, calculate
#'   unconditional type 2 response probabilities as an unweighted average over
#'   type 1 responses.
#' @returns a tibble containing posterior draws of model parameters with the
#'   following columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: for `epred_draws_metad`, identifiers for the posterior sample
#'  * `{.stimulus}`, `{.response}`, `{.confidence}`: identifiers for the response type
#'  * `.epred`: probability of the type 1 and type 2 response given the stimulus, \eqn{P(R, C \;\vert\; S)}
#' @examples
#' \donttest{
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # obtain model predictions
#' # equivalent to `add_type2_draws(newdata, example_model())`
#' type2_draws(example_model(), newdata)
#'
#' # obtain model predictions (`posterior::rvar`)
#' # equivalent to `add_type2_rvars(newdata, example_model(), by_stimulus = FALSE)`
#' type2_rvars(example_model(), newdata, by_stimulus = FALSE)
#' }
#' @seealso [type2_probabilities()]
#' @rdname type2_draws
#' @export
type2_draws <- function(
  object, newdata, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", by_stimulus = TRUE, by_response = TRUE
) {
  ## grouping columns
  .stimulus <- get_stimulus(object, .default = .stimulus)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, .response, .confidence
  ))]

  draws <- epred_draws_metad(object, newdata) |>
    group_by(.data$.row, !!!syms(.cols), !!sym(.stimulus), !!sym(.response), .data$.draw) |>
    mutate(.epred = .data$.epred / sum(.data$.epred)) |>
    group_by(.data$.row, !!!syms(.cols))

  if (by_stimulus) {
    draws <- draws |> group_by(!!sym(.stimulus), .add = TRUE)
  }
  if (by_response) {
    draws <- draws |> group_by(!!sym(.response), .add = TRUE)
  }

  draws |>
    group_by(!!sym(.confidence), .data$.draw, .add = TRUE) |>
    summarize(.epred = mean(.data$.epred))
}

#' @rdname type2_draws
#' @export
add_type2_draws <- function(newdata, object, ...) {
  type2_draws(object, newdata, ...)
}

#' @rdname type2_draws
#' @export
type2_rvars <- function(
  object, newdata, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", by_stimulus = TRUE, by_response = TRUE
) {
  ## grouping columns
  .stimulus <- get_stimulus(object, .default = .stimulus)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, .response, .confidence
  ))]

  draws <- epred_rvars_metad(object, newdata) |>
    group_by(.data$.row, !!!syms(.cols), !!sym(.stimulus), !!sym(.response)) |>
    mutate(.epred = .data$.epred / rvar_sum(.data$.epred)) |>
    group_by(.data$.row, !!!syms(.cols))

  if (by_stimulus) {
    draws <- draws |> group_by(!!sym(.stimulus), .add = TRUE)
  }
  if (by_response) {
    draws <- draws |> group_by(!!sym(.response), .add = TRUE)
  }

  draws |>
    group_by(!!sym(.confidence), .add = TRUE) |>
    summarize(.epred = rvar_mean(.data$.epred))
}

#' @rdname type2_draws
#' @export
add_type2_rvars <- function(newdata, object, ...) {
  type2_rvars(object, newdata, ...)
}

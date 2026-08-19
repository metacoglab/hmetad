#' Calculate the area under empirical type 2 receiver operating characteristic curves
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
#' @param by_response If `TRUE` (default), calculate type 2 ROCs conditional on
#'   type 1 response.
#' @returns A tibble with columns:
#'  * `...`: the grouping columns in `data`
#'  * `{.response}` (if `by_response=TRUE`): the type 1 response
#'  * `auroc2`: the area under the type 2 ROC
#' @seealso [roc2()], [auroc2_draws()], [auroc2_rvars()]
#' @examples
#' # calculate type 2 ROCs by stimulus
#' auroc2(example_data())
#'
#' # calculate type 2 ROCs by condition, averaging over type 1 responses
#' auroc2(sim_metad_condition(), condition, by_response = FALSE)
#' @export
auroc2 <- function(
  data, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", .joint_response = "joint_response",
  K = NULL, by_response = TRUE
) {
  roc2(
    data, ...,
    .stimulus = .stimulus, .response = .response,
    .confidence = .confidence, .joint_response = .joint_response, K = K,
    by_response = by_response, bounds = TRUE
  ) |>
    arrange(desc(!!sym(.confidence))) |>
    summarize(auroc2 = sum(diff(c(0, .data$p_fa2)) *
      (.data$p_hit2 + lag(.data$p_hit2, default = 0))) / 2)
}


#' Obtain posterior draws of the area under the type 2 receiver operating
#' characteristic (ROC) curve.
#'
#' @description Given a data frame and a meta-d' model, adds estimates of AUROC2
#'   (optionally for each type 1 response). For `auroc2_draws` and
#'   `add_auroc2_draws`, estimates are returned in a tidy tibble with one row
#'   per posterior draw. For `auroc2_rvars` and `add_auroc2_rvars`, parameters
#'   are returned as `posterior::rvar`s, with one row per row in `newdata`.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional parameters passed to [tidybayes::epred_draws]
#' @param .response The name of "response" column
#' @param .confidence The name of "confidence" column
#' @param by_response If `TRUE` (default), compute separate ROCs for each type 1
#'   response. Otherwise, average ROCs across both type 1 responses.
#' @returns a tibble containing posterior draws of the pseudo type 1 ROC with
#'   the following columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: for `auroc2_draws` and `add_auroc2_draws`, identifiers for the posterior sample
#'  * `{.response}`: the type 1 response for perceived stimulus presence (\eqn{R \in \{0, 1\}})
#'  * `{.confidence}`: the type 2 confidence response (\eqn{C \in [1, K]})
#'  * `p_fa2`: the cumulative probability of an incorrect response (\eqn{P(C\ge c \;\vert\; R\ne S)})
#'  * `p_hit2`: the cumulative probability of a correct response (\eqn{P(C\ge c \;\vert\; R = S)})
#' @rdname auroc2_draws
#' @seealso [auroc2()], [tidybayes::epred_draws()], [tidybayes::epred_rvars()]
#' @examples
#' \donttest{
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # compute type 2 ROC curve
#' # equivalent to `add_auroc2_draws(newdata, example_model())`
#' auroc2_draws(example_model(), newdata)
#'
#' # use posterior::rvar for additional efficiency
#' # equivalent to `add_auroc2_rvars(newdata, example_model())`
#' auroc2_rvars(example_model(), newdata)
#' }
#' @export
auroc2_draws <- function(
  object, newdata, ..., .response = "response", .confidence = "confidence",
  bounds = FALSE, by_response = TRUE
) {
  ## grouping columns
  .stimulus <- get_stimulus(object, .default = list2(...)$.stimulus)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, .response, .confidence
  ))]

  draws <- roc2_draws(
    object, newdata, ...,
    .response = .response,
    .confidence = .confidence, bounds = TRUE
  ) |>
    group_by(.data$.row, !!!syms(.cols))

  if (by_response) {
    draws <- draws |> group_by(!!sym(.response), .add = TRUE)
  }

  draws |>
    group_by(.data$.draw, .add = TRUE) |>
    arrange(desc(!!sym(.confidence))) |>
    summarize(auroc2 = sum(diff(c(0, .data$p_fa2)) *
      (.data$p_hit2 + lag(.data$p_hit2, default = 0))) / 2)
}

#' @rdname auroc2_draws
#' @export
add_auroc2_draws <- function(newdata, object, ...) {
  auroc2_draws(object, newdata, ...)
}

#' @rdname auroc2_draws
#' @export
auroc2_rvars <- function(
  object, newdata, ..., .response = "response", .confidence = "confidence",
  bounds = FALSE, by_response = TRUE
) {
  ## grouping columns
  .stimulus <- get_stimulus(object, .default = list2(...)$.stimulus)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, .response, .confidence
  ))]

  draws <- roc2_rvars(
    object, newdata, ...,
    .response = .response,
    .confidence = .confidence, bounds = TRUE
  ) |>
    group_by(.data$.row, !!!syms(.cols))

  if (by_response) {
    draws <- draws |> group_by(!!sym(.response), .add = TRUE)
  }

  draws |>
    arrange(desc(!!sym(.confidence))) |>
    summarize(auroc2 = rvar_sum(diff(c(as_rvar(0), .data$p_fa2)) *
      (.data$p_hit2 + lag(.data$p_hit2, default = as_rvar(0)))) / 2)
}

#' @rdname auroc2_draws
#' @export
add_auroc2_rvars <- function(newdata, object, ...) {
  auroc2_rvars(object, newdata, ...)
}

#' Calculate area under the empirical pseudo-type 1 receiver operating characteristic curve
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
#' @returns A tibble with columns:
#'  * `...`: the grouping columns in `data`
#'  * `auroc1`: the area under the pseudo type 1 ROC curve
#' @seealso [roc1()], [auroc1_draws()], [auroc1_rvars()]
#' @examples
#' # calculate area under the type 1 ROC
#' auroc1(example_data())
#'
#' # calculate type 1 ROCs by condition
#' auroc1(sim_metad_condition(), condition)
#' @export
auroc1 <- function(
  data, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", .joint_response = "joint_response", K = NULL
) {
  roc1(
    data, ...,
    .stimulus = .stimulus, .response = .response,
    .confidence = .confidence, .joint_response = .joint_response, K = K, bounds = TRUE
  ) |>
    arrange(desc(!!sym(.joint_response))) |>
    summarize(auroc1 = sum(diff(c(0, .data$p_fa)) *
      (.data$p_hit + lag(.data$p_hit, default = 0))) / 2)
}

#' Obtain posterior draws of the area under the pseudo type 1 receiver operating
#' characteristic (ROC) curve.
#'
#' @description Given a data frame and a meta-d' model, adds estimates of the
#'   area under the type 1 ROC curve. For `auroc1_draws` and `add_auroc1_draws`,
#'   estimates are returned in a tidy tibble with one row per posterior draw.
#'   For `auroc1_rvars` and `add_auroc1_rvars`, parameters are returned as
#'   [posterior::rvar]s, with one row per row in `newdata`.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional parameters passed to [tidybayes::epred_draws] or
#'   [tidybayes::epred_rvars]
#' @param .response The name of "response" column
#' @param .confidence The name of "confidence" column
#' @param .joint_response The name of "joint_response" column
#' @returns a tibble containing posterior draws of the area under the pseudo
#'   type 1 ROC with the following columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: for `auroc1_draws` and `add_auroc1_draws`, identifiers for the posterior sample
#'  * `auroc1`: the area under the pseudo type 1 ROC curve
#' @rdname auroc1_draws
#' @seealso [auroc1()], [roc1_draws()], [roc1_rvars()]
#' @examples
#' \donttest{
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # compute pseudo-type 1 ROC curve
#' # equivalent to `auroc1_draws(example_model(), newdata)`
#' add_auroc1_draws(newdata, example_model())
#'
#' # use posterior::rvar for additional efficiency
#' # equivalent to `add_auroc1_draws(newdata, example_model())`
#' auroc1_rvars(example_model(), newdata)
#' }
#' @export
auroc1_draws <- function(
  object, newdata, ..., .response = "response",
  .confidence = "confidence", .joint_response = "joint_response"
) {
  ## grouping columns
  .stimulus <- get_stimulus(object, .default = list2(...)$.stimulus)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, .joint_response,
    .response, .confidence
  ))]

  roc1_draws(
    object, newdata, ...,
    .response = .response, .confidence = .confidence,
    .joint_response = .joint_response, bounds = TRUE
  ) |>
    group_by(.data$.row, !!!syms(.cols), .data$.draw) |>
    arrange(desc(!!sym(.joint_response))) |>
    summarize(auroc1 = sum(diff(c(0, .data$p_fa)) *
      (.data$p_hit + lag(.data$p_hit, default = 0))) / 2)
}

#' @rdname auroc1_draws
#' @export
add_auroc1_draws <- function(
  newdata, object, ...
) {
  auroc1_draws(object, newdata, ...)
}

#' @rdname auroc1_draws
#' @export
auroc1_rvars <- function(
  object, newdata, ..., .response = "response",
  .confidence = "confidence", .joint_response = "joint_response"
) {
  ## grouping columns
  .stimulus <- get_stimulus(object, .default = list2(...)$.stimulus)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, .joint_response,
    .response, .confidence
  ))]

  roc1_rvars(
    object, newdata, ...,
    .response = .response, .confidence = .confidence,
    .joint_response = .joint_response, bounds = TRUE
  ) |>
    group_by(.data$.row, !!!syms(.cols)) |>
    arrange(desc(!!sym(.joint_response))) |>
    summarize(auroc1 = rvar_sum(diff(c(as_rvar(0), .data$p_fa)) *
      (.data$p_hit + lag(.data$p_hit, default = as_rvar(0)))) / 2)
}

#' @rdname auroc1_draws
#' @export
add_auroc1_rvars <- function(
  newdata, object, ...
) {
  auroc1_rvars(object, newdata, ...)
}

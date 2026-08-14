#' Calculate empirical pseudo-type 1 receiver operating characteristic curves
#'
#' Given a dataset `data`, determine the cumulative probability of each
#' combination of type 1 and type 2 responses conditional on stimulus.
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
#' @param bounds If `TRUE`, include the endpoints of the ROC at \eqn{(0, 0)} and
#'   \eqn{(1, 1)}. Otherwise, the endpoints are excluded.
#' @returns A tibble with columns:
#'  * `...`: the grouping columns in `data`
#'  * `{.response}`: the type 1 response
#'  * `{.confidence}`: the type 2 response
#'  * `{.joint_response}`: the joint type 1/type 2 response
#'  * `n_0`: the number of rows in `data` with `stimulus=0` and the corresponding joint response
#'  * `n_1`: the number of rows in `data` with `stimulus=1` and the corresponding joint response
#'  * `p_0`: where `stimulus=0`, the proportion of rows in `data` with joint response equal to `.joint_response`
#'  * `p_1`: where `stimulus=1`, the proportion of rows in `data` with joint response equal to `.joint_response`
#'  * `p_fa`: where `stimulus=0`, the proportion of rows in `data` with joint response greater than `.joint_response`
#'  * `p_hit`: where `stimulus=1`, the proportion of rows in `data` with joint response greater than `.joint_response`
#' @seealso [roc1_draws()], [roc1_rvars()]
#' @examples
#' # calculate type 1 ROCs by stimulus
#' roc1(example_data())
#'
#' # calculate type 1 ROCs by condition
#' roc1(sim_metad_condition(), condition)
#' @export
roc1 <- function(
  data, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", .joint_response = "joint_response", K = NULL,
  bounds = FALSE
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

  data <- data |>
    joint_probabilities(
      ...,
      .stimulus = .stimulus, .response = .response,
      .confidence = .confidence, .joint_response = .joint_response,
      K = K, by_stimulus = TRUE
    ) |>
    ungroup() |>
    tidyr::pivot_wider(names_from = .stimulus, values_from = c("n", "p"))

  ## add in ROC bounds
  expansion <- NULL
  if (length(enquos(...)) > 0) {
    expansion <- tidyr::expand(
      data, distinct(data, ...),
      "{.response}" := 0L, "{.confidence}" := K + 1L,
      "{.joint_response}" := 0L,
      n_0 = 0L, n_1 = 0L, p_0 = 0, p_1 = 0
    )
  } else {
    expansion <- tidyr::expand(
      data, ...,
      "{.response}" := 0L, "{.confidence}" := K + 1L,
      "{.joint_response}" := 0L,
      n_0 = 0L, n_1 = 0L, p_0 = 0, p_1 = 0
    )
  }

  data <- tidyr::complete(data, expansion) |>
    arrange(..., !!sym(.joint_response)) |>
    group_by(...) |>
    mutate(
      p_fa = 1 - cumsum(.data$p_0),
      p_hit = 1 - cumsum(.data$p_1)
    )

  if (!bounds) {
    data <- data |>
      filter(!!sym(.joint_response) %in% seq_len(2 * K - 1))
  }

  data
}


#' Obtain posterior draws of the pseudo type 1 receiver operating characteristic (ROC) curve.
#'
#' @description Given a data frame and a meta-d' model, adds estimates of the
#' cumulative probability over joint_responses.
#' For `roc1_draws` and `add_roc1_draws`, estimates are returned in a tidy
#' tibble with one row per posterior draw and per joint response.
#' For `roc1_rvars` and `add_roc1_rvars`, parameters are returned as
#' [posterior::rvar]s, with one row per row in `newdata` and per joint response.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional parameters passed to [tidybayes::epred_draws] or [tidybayes::epred_rvars]
#' @param .response The name of "response" column
#' @param .confidence The name of "confidence" column
#' @param .joint_response The name of "joint_response" column
#' @param bounds If `TRUE`, include the endpoints of the ROC at \eqn{(0, 0)} and \eqn{(1, 1)}.
#' Otherwise, the endpoints are excluded.
#' @returns a tibble containing posterior draws of the pseudo type 1 ROC with the following
#' columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: for `roc1_draws` and `add_roc1_draws`, identifiers for the posterior sample
#'  * `{.joint_response}`: the combined type 1 / type 2 response (\eqn{J \in [1, 2K]}) for \eqn{K} confidence levels)
#'  * `{.response}`: the type 1 response for perceived stimulus presence (\eqn{R \in \{0, 1\}})
#'  * `{.confidence}`: the type 2 confidence response (\eqn{C \in [1, K]})
#'  * `p_fa`: the cumulative probability of a 'present'/'old' response for `stimulus==0` (\eqn{P(J \ge j \;\vert\; S=0)})
#'  * `p_hit`: the cumulative probability of a 'present'/'old' response for `stimulus==1` (\eqn{P(J \ge j \;\vert\; S=1)})
#' @rdname roc1_draws
#' @seealso [roc1()], [tidybayes::epred_draws()], [tidybayes::epred_rvars()]
#' @examples
#' \donttest{
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # compute pseudo-type 1 ROC curve
#' # equivalent to ``
#' roc1_draws(example_model(), newdata)
#' add_roc1_draws(newdata, example_model())
#'
#' # use posterior::rvar for additional efficiency
#' # equivalent to `add_roc1_draws(newdata, example_model())`
#' roc1_rvars(example_model(), newdata)
#'
#' # include the ROC bounds
#' # equivalent to `add_roc1_draws(newdata, example_model(), bounds = TRUE)`
#' roc1_draws(example_model(), newdata, bounds = TRUE)
#' }
#' @export
roc1_draws <- function(
  object, newdata, ..., .response = "response",
  .confidence = "confidence", .joint_response = "joint_response", bounds = FALSE
) {
  draws <- epred_draws_metad(
    object, newdata, ...,
    .response = .response,
    .confidence = .confidence, .joint_response = .joint_response
  )

  ## grouping columns
  .stimulus <- get_stimulus(object)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, .joint_response,
    .response, .confidence
  ))]

  ## number of confidence levels
  K <- as.integer(n_distinct(pull(draws, .joint_response)) / 2)

  ## calculate cumulative probabilities
  draws <- draws |>
    filter(!!sym(.joint_response) < 2 * K) |>
    group_by(.data$.row, !!sym(.stimulus), .data$.draw) |>
    mutate(.epred = 1 - cumsum(.data$.epred)) |>
    tidyr::pivot_wider(
      names_from = !!sym(.stimulus), values_from = ".epred",
      names_prefix = "p_"
    ) |>
    rename(p_hit = "p_1", p_fa = "p_0") |>
    group_by(
      .data$.row, !!!syms(.cols), !!sym(.joint_response),
      !!sym(.response), !!sym(.confidence)
    )

  if (bounds) {
    ## add (0, 0) and (1, 1) points to ROC
    draws <- draws |>
      bind_rows(draws |>
        ungroup() |>
        distinct(
          .data$.row, !!!syms(.cols),
          .data$.chain, .data$.iteration, .data$.draw
        ) |>
        tidyr::expand_grid(tibble(
          "{.joint_response}" := c(0, K * 2L),
          "{.response}" := c(0L, 1L),
          "{.confidence}" := c(K + 1L, K),
          p_fa = c(1, 0),
          p_hit = c(1, 0)
        ))) |>
      arrange(!!sym(.joint_response))
  }

  draws
}

#' @rdname roc1_draws
#' @export
add_roc1_draws <- function(newdata, object, ...) {
  roc1_draws(object, newdata, ...)
}

#' @rdname roc1_draws
#' @export
roc1_rvars <- function(
  object, newdata, ..., .response = "response",
  .confidence = "confidence", .joint_response = "joint_response", bounds = FALSE
) {
  draws <- epred_rvars_metad(
    object, newdata, ...,
    .response = .response,
    .confidence = .confidence, .joint_response = .joint_response
  )

  ## grouping columns
  .stimulus <- get_stimulus(object)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, .joint_response,
    .response, .confidence
  ))]

  ## number of confidence levels
  K <- as.integer(n_distinct(pull(draws, .joint_response)) / 2)

  ## calculate cumulative probabilities
  draws <- draws |>
    filter(!!sym(.joint_response) < 2 * K) |>
    group_by(.data$.row, !!sym(.stimulus)) |>
    mutate(.epred = 1 - cumsum(.data$.epred)) |>
    tidyr::pivot_wider(
      names_from = !!sym(.stimulus), values_from = ".epred",
      names_prefix = "p_"
    ) |>
    rename(p_hit = "p_1", p_fa = "p_0") |>
    group_by(
      .data$.row, !!!syms(.cols), !!sym(.joint_response),
      !!sym(.response), !!sym(.confidence)
    )

  if (bounds) {
    ## add (0, 0) and (1, 1) points to ROC
    draws <- draws |>
      bind_rows(draws |>
        ungroup() |>
        distinct(.data$.row, !!!syms(.cols)) |>
        tidyr::expand_grid(tibble(
          "{.joint_response}" := c(0L, K * 2L),
          "{.response}" := c(0L, 1L),
          "{.confidence}" := c(K + 1L, K),
          p_fa = c(1, 0),
          p_hit = c(1, 0)
        ))) |>
      arrange(!!sym(.joint_response))
  }

  draws
}

#' @rdname roc1_draws
#' @export
add_roc1_rvars <- function(newdata, object, ...) {
  roc1_rvars(object, newdata, ...)
}

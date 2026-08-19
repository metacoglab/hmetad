#' Calculate empirical type 2 receiver operating characteristic curve
#'
#' Given a dataset `data`, determine the cumulative probability of each type 2
#' responses conditional on accuracy, optionally conditional on type 1 response.
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
#' @param bounds If `TRUE`, include the endpoints of the ROC at \eqn{(0, 0)} and
#'   \eqn{(1, 1)}. Otherwise, the endpoints are excluded.
#' @returns A tibble with columns:
#'  * `...`: the grouping columns in `data`
#'  * `{.response}` (if `by_response=TRUE`): the type 1 response
#'  * `{.confidence}`: the type 2 response
#'  * `n_0`: the number of rows in `data` with `stimulus=0` and the corresponding `joint_response`
#'  * `n_1`: the number of rows in `data` with `stimulus=1` and the corresponding `joint_response`
#'  * `p_0`: for incorrect trials, the proportion of rows in `data` with confidence equal to `confidence`
#'  * `p_1`: for correct trials the proportion of rows in `data` with confidence equal to `confidence`
#'  * `p_fa2`: for incorrect trials, the proportion of rows in `data` with confidence greater than `confidence`
#'  * `p_hit2`: for correct trials, the proportion of rows in `data` with confidence greater than `confidence`
#' @seealso [roc2_draws()], [roc2_rvars()]
#' @examples
#' # calculate type 2 ROCs by stimulus
#' roc2(example_data())
#'
#' # calculate type 2 ROCs by condition, averaging over type 1 responses
#' roc2(sim_metad_condition(), condition, by_response = FALSE)
#' @export
roc2 <- function(
  data, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", .joint_response = "joint_response", K = NULL,
  bounds = FALSE, by_response = TRUE
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
    mutate(accuracy = as.integer(!!sym(.stimulus) == !!sym(.response))) |>
    group_by(..., !!sym(.response)) |>
    select(-!!sym(.stimulus), -!!sym(.joint_response)) |>
    tidyr::pivot_wider(names_from = "accuracy", values_from = c("n", "p")) |>
    arrange(..., !!sym(.response), !!sym(.confidence)) |>
    mutate(
      p_hit2 = 1 - cumsum(.data$p_1) / sum(.data$p_1),
      p_fa2 = 1 - cumsum(.data$p_0) / sum(.data$p_0)
    ) |>
    ungroup()

  ## add in ROC bounds
  expansion <- NULL
  if (length(enquos(...)) > 0) {
    expansion <- tidyr::expand(
      data, distinct(data, ...), "{.response}" := 0:1, "{.confidence}" := 0L,
      n_0 = 0L, n_1 = 0L, p_0 = 0, p_1 = 0, p_hit2 = 1, p_fa2 = 1
    )
  } else {
    expansion <- tidyr::expand(
      data, "{.response}" := 0:1, "{.confidence}" := 0L,
      n_0 = 0L, n_1 = 0L, p_0 = 0, p_1 = 0, p_hit2 = 1, p_fa2 = 1
    )
  }

  data <- tidyr::complete(data, expansion) |>
    arrange(..., !!sym(.response), !!sym(.confidence)) |>
    group_by(..., !!sym(.response))

  if (!by_response) {
    data <- data |>
      group_by(..., !!sym(.confidence)) |>
      summarize(
        n_0 = sum(.data$n_0), n_1 = sum(.data$n_1),
        p_0 = mean(.data$p_0), p_1 = mean(.data$p_1),
        p_fa2 = mean(.data$p_fa2), p_hit2 = mean(.data$p_hit2)
      )
  }

  if (!bounds) {
    data <- data |>
      filter(!!sym(.confidence) %in% seq_len(K - 1))
  }

  data
}


#' Obtain posterior draws of the response-specific type 2 receiver operating
#' characteristic (ROC) curves.
#'
#' @description Given a data frame and a meta-d' model, adds estimates of the
#'   cumulative probability over confidence for each type 1 response. For
#'   `roc2_draws` and `add_roc2_draws`, estimates are returned in a tidy tibble
#'   with one row per posterior draw and per joint response. For `roc2_rvars`
#'   and `add_roc2_rvars`, parameters are returned as `posterior::rvar`s, with
#'   one row per row in `newdata` and per joint response.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional parameters passed to [tidybayes::epred_draws]
#' @param .response The name of "response" column
#' @param .confidence The name of "confidence" column
#' @param bounds If `TRUE`, include the endpoints of the ROC at \eqn{(0, 0)} and
#'   \eqn{(1, 1)}. Otherwise, the endpoints are excluded.
#' @param by_response If `TRUE` (default), compute separate ROCs for each type 1
#'   response. Otherwise, average ROCs across both type 1 responses.
#' @returns a tibble containing posterior draws of the pseudo type 1 ROC with
#'   the following columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: for `roc2_draws` and `add_roc2_draws`, identifiers for the posterior sample
#'  * `{.response}`: the type 1 response for perceived stimulus presence (\eqn{R \in \{0, 1\}})
#'  * `{.confidence}`: the type 2 confidence response (\eqn{C \in [1, K]})
#'  * `p_fa2`: the cumulative probability of an incorrect response (\eqn{P(C\ge c \;\vert\; R\ne S)})
#'  * `p_hit2`: the cumulative probability of a correct response (\eqn{P(C\ge c \;\vert\; R = S)})
#' @rdname roc2_draws
#' @seealso [roc2()], [tidybayes::epred_draws()], [tidybayes::epred_rvars()]
#' @examples
#' \donttest{
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # compute type 2 ROC curve
#' # equivalent to `add_roc2_draws(newdata, example_model())`
#' roc2_draws(example_model(), newdata)
#'
#' # use posterior::rvar for additional efficiency
#' # equivalent to `add_roc2_rvars(newdata, example_model())`
#' roc2_rvars(example_model(), newdata)
#'
#' # include the ROC bounds
#' # equivalent to `roc2_draws(newdata, example_model(), bounds = TRUE)`
#' roc2_draws(example_model(), newdata, bounds = TRUE)
#' }
#' @export
roc2_draws <- function(
  object, newdata, ..., .response = "response", .confidence = "confidence",
  bounds = FALSE, by_response = TRUE
) {
  draws <- epred_draws_metad(object, newdata, ..., .response = .response, .confidence = .confidence)

  ## grouping columns
  .stimulus <- get_stimulus(object, .default = list2(...)$.stimulus)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, "joint_response",
    .response, .confidence
  ))]

  ## number of confidence levels
  K <- as.integer(n_distinct(draws$joint_response) / 2)

  ## calculate cumulative probabilities
  draws <- draws |>
    mutate(
      "{.response}" := type1_response(.data$joint_response, K),
      "{.confidence}" := type2_response(.data$joint_response, K),
      accuracy = as.integer(!!sym(.stimulus) == !!sym(.response))
    ) |>
    group_by(.data$.row, .data$.draw, .data$accuracy, !!sym(.response)) |>
    mutate(
      .epred = cumsum(.data$.epred) / sum(.data$.epred),
      .epred = ifelse(!!sym(.response), 1 - .data$.epred, .data$.epred)
    ) |>
    filter(
      !(!!sym(.response) == 0 & !!sym(.confidence) == 1),
      !(!!sym(.response) == 1 & !!sym(.confidence) == K)
    ) |>
    mutate("{.confidence}" := ifelse(!!sym(.response) == 0, !!sym(.confidence) - 1L, !!sym(.confidence))) |>
    select(-"joint_response", -!!sym(.stimulus)) |>
    ungroup() |>
    tidyr::pivot_wider(
      names_from = "accuracy",
      values_from = ".epred", names_prefix = "p_"
    ) |>
    rename(p_hit2 = "p_1", p_fa2 = "p_0") |>
    group_by(.data$.row, !!!syms(.cols), !!sym(.response), !!sym(.confidence))

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
          "{.response}" := c(0L, 0L, 1L, 1L),
          "{.confidence}" := c(0L, K, 0L, K),
          p_fa2 = c(1, 0, 1, 0),
          p_hit2 = c(1, 0, 1, 0)
        )))
  }

  if (!by_response) {
    draws <- draws |>
      group_by(.data$.row, !!!syms(.cols), !!sym(.confidence), .data$.draw) |>
      summarize(
        p_hit2 = mean(.data$p_hit2),
        p_fa2 = mean(.data$p_fa2)
      ) |>
      group_by(.data$.row, !!!syms(.cols), !!sym(.confidence))
  }

  draws
}

#' @rdname roc2_draws
#' @export
add_roc2_draws <- function(newdata, object, ...) {
  roc2_draws(object, newdata, ...)
}

#' @rdname roc2_draws
#' @export
roc2_rvars <- function(
  object, newdata, ..., .response = "response", .confidence = "confidence",
  bounds = FALSE, by_response = TRUE
) {
  draws <- epred_rvars_metad(
    object, newdata, ...,
    .response = .response, .confidence = .confidence
  )

  ## grouping columns
  .stimulus <- get_stimulus(object, .default = list2(...)$.stimulus)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, "joint_response",
    .response, .confidence
  ))]

  ## number of confidence levels
  K <- as.integer(n_distinct(draws$joint_response) / 2)

  draws <- draws |>
    mutate(accuracy = as.integer(!!sym(.stimulus) == !!sym(.response))) |>
    group_by(.data$.row, .data$accuracy, !!sym(.response)) |>
    mutate(
      .epred = cumsum(.data$.epred) / posterior::rvar_sum(.data$.epred),
      .epred = rvar_ifelse(!!sym(.response) == 1, 1 - .data$.epred, .data$.epred)
    ) |>
    filter(
      !(!!sym(.response) == 0 & !!sym(.confidence) == 1),
      !(!!sym(.response) == 1 & !!sym(.confidence) == K)
    ) |>
    mutate("{.confidence}" := ifelse(!!sym(.response) == 0, !!sym(.confidence) - 1L, !!sym(.confidence))) |>
    select(-"joint_response", -!!sym(.stimulus)) |>
    ungroup() |>
    tidyr::pivot_wider(
      names_from = "accuracy",
      values_from = ".epred", names_prefix = "p_"
    ) |>
    rename(p_hit2 = "p_1", p_fa2 = "p_0") |>
    group_by(.data$.row, !!!syms(.cols), !!sym(.response), !!sym(.confidence)) |>
    arrange(.data$.row, !!!syms(.cols), !!sym(.response), !!sym(.confidence))

  if (bounds) {
    ## add (0, 0) and (1, 1) points to ROC
    draws <- draws |>
      bind_rows(draws |>
        ungroup() |>
        distinct(.data$.row, !!!syms(.cols)) |>
        tidyr::expand_grid(tibble(
          "{.response}" := c(0L, 0L, 1L, 1L),
          "{.confidence}" := c(0L, K, 0L, K),
          p_fa2 = c(1, 0, 1, 0),
          p_hit2 = c(1, 0, 1, 0)
        ))) |>
      arrange(!!sym(.response), !!sym(.confidence))
  }

  if (!by_response) {
    draws <- draws |>
      group_by(.data$.row, !!!syms(.cols), !!sym(.confidence)) |>
      summarize(
        p_hit2 = rvar_mean(.data$p_hit2),
        p_fa2 = rvar_mean(.data$p_fa2)
      ) |>
      group_by(.data$.row, !!!syms(.cols), !!sym(.confidence))
  }

  draws
}

#' @rdname roc2_draws
#' @export
add_roc2_rvars <- function(newdata, object, ...) {
  roc2_rvars(object, newdata, ...)
}

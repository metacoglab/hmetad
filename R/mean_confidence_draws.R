#' Calculate empirical mean confidence
#'
#' Given a dataset `data`, determine the mean confidence rating, optionally
#' conditional on stimulus and/or type 1 response.
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
#' @param by_stimulus If `TRUE` (default), calculate mean confidence conditional
#'   on stimulus. Ignored if `by_correct=TRUE`.
#' @param by_response If `TRUE` (default), calculate mean confidence conditional
#'   on type 2 response. Ignored if `by_correct=TRUE`.
#' @param by_correct If `FALSE` (default), calculate mean confidence conditional
#'   on stimulus and/or type 1 response. If `TRUE`, instead calculate mean
#'   confidence conditional on accuracy.
#' @returns A tibble with columns:
#'  * `...`: the grouping columns in `data`
#'  * `{.stimulus}`: the stimulus (if `by_stimulus=TRUE`)
#'  * `{.response}`: the type 1 response (if `by_response=TRUE`)
#'  * `correct`: the accuracy (if `by_correct=TRUE`)
#'  * `mean_confidence`: the mean confidence rating
#' @seealso [mean_confidence_draws()], [mean_confidence_rvars()]
#' @examples
#' # calculate mean confidence by stimulus and response
#' mean_confidence(example_data())
#'
#' # calculate mean confidence by accuracy
#' mean_confidence(example_data(), by_correct = TRUE)
#'
#' # calculate mean confidence by condition, averaging over type 1 responses
#' mean_confidence(sim_metad_condition(), condition, by_response = FALSE)
#' @export
mean_confidence <- function(
  data, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", .joint_response = "joint_response",
  K = NULL, by_stimulus = TRUE, by_response = TRUE, by_correct = FALSE
) {
  data <- .aggregate_metad(
    data, ...,
    .stimulus = .stimulus, .response = .response,
    .confidence = .confidence, .joint_response = .joint_response, K = K
  ) |>
    group_by(...)

  if (by_correct) {
    data <- data |>
      mutate(correct = as.integer(!!sym(.stimulus) == !!sym(.response))) |>
      group_by(.data$correct, .add = TRUE)
  } else {
    if (by_stimulus) {
      data <- data |> group_by(!!sym(.stimulus), .add = TRUE)
    }
    if (by_response) {
      data <- data |> group_by(!!sym(.response), .add = TRUE)
    }
  }

  data |>
    mutate(p = .data$n / sum(.data$n)) |>
    summarize(mean_confidence = sum(!!sym(.confidence) * .data$p))
}


#' Obtain posterior draws of mean confidence
#'
#' @description Computes posterior mean confidence conditional on stimulus and
#'   response (\eqn{\mathbb{E}[C \;\vert\; S=s,R=r]}), stimulus (averaging over
#'   responses, \eqn{\mathbb{E}[C \;\vert\; S=s]}), response (averaging over
#'   stimuli, \eqn{\mathbb{E}[C \;\vert\; R=r]}), neither (averaging over
#'   stimuli and responses, \eqn{\mathbb{E}[C]}), or accuracy (\eqn{\mathbb{E}[C
#'   \;\vert\; A=(r=s)]}). For `mean_confidence_draws` and
#'   `add_mean_confidence_draws`, estimates are returned in a tidy tibble with
#'   one row per posterior draw, stimulus, and response. For
#'   `mean_confidence_rvars` and `add_mean_confidence_rvars`, estimates are
#'   returned as [posterior::rvar]s, with one row per row in `newdata`.
#'
#'   `add_mean_confidence_draws` is an alias of `mean_confidence_draws` with
#'   argument order swapped.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional arguments to [tidybayes::epred_draws] or
#'   [tidybayes::epred_rvars]
#' @param .stimulus The name of "stimulus" column
#' @param .response The name of "response" column
#' @param by_stimulus If TRUE, predict mean confidence separately by stimulus.
#'   Otherwise, predict mean confidence averaging over stimuli. Ignored if
#'   `by_correct==TRUE`.
#' @param by_response If TRUE, predict mean confidence separately by response
#'   Otherwise, predict mean confidence averaging over responses. Ignored if
#'   `by_correct==TRUE`.
#' @param by_correct If TRUE, predict mean confidence separately for correct and
#'   incorrect responses.
#' @returns a tibble containing posterior draws of mean confidence with the
#'   following columns:
#'   * `.row`: the row of `newdata`
#'   * `.chain`, `.iteration`, `.draw`: for `mean_confidence_draws` and `add_mean_confidence_draws`, identifiers for the posterior sample
#'   * `{.stimulus}`: indicator for stimulus presence (if `by_stimulus==TRUE & by_correct==FALSE`)
#'   * `{.response}`: indicator for type 1 response (if `by_response==TRUE & by_correct==FALSE`)
#'   * `correct`: indicator for the accuracy of the type 1 response (if `by_correct==TRUE`)
#'   * `.epred`: the predicted mean confidence
#' @rdname mean_conf_draws
#' @seealso [mean_confidence()], [tidybayes::epred_draws()], [tidybayes::epred_rvars()]
#' @examples
#' \donttest{
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # compute mean confidence by stimulus and response
#' # equivalent to `add_mean_confidence_draws(newdata, example_model())`
#' mean_confidence_draws(example_model(), newdata)
#'
#' # compute mean confidence by stimulus
#' # equivalent to `add_mean_confidence_draws(newdata, example_model(), by_response = FALSE)`
#' mean_confidence_draws(example_model(), newdata, by_response = FALSE)
#'
#' # compute mean confidence by response
#' # equivalent to `add_mean_confidence_draws(newdata, example_model(), by_stimulus = FALSE)`
#' mean_confidence_draws(example_model(), newdata, by_stimulus = FALSE)
#'
#' # compute mean confidence by accuracy
#' # equivalent to `add_mean_confidence_draws(newdata, example_model(), by_correct = TRUE)`
#' mean_confidence_draws(example_model(), newdata, by_correct = TRUE)
#'
#' # compute mean confidence averaging over stimuli and responses
#' # equivalent to `add_mean_confidence_draws(newdata, example_model(), ...)`
#' mean_confidence_draws(example_model(), newdata, by_stimulus = FALSE, by_response = FALSE)
#'
#' # use `posterior::rvar` for increased efficiency
#' # equivalent to `add_mean_confidence_rvars(newdata, example_model())`
#' mean_confidence_rvars(example_model(), newdata)
#' }
#' @export
mean_confidence_draws <- function(object, newdata, ...,
                                  .stimulus = "stimulus", .response = "response",
                                  by_stimulus = TRUE, by_response = TRUE,
                                  by_correct = FALSE) {
  ## grouping columns
  .stimulus <- get_stimulus(object, .stimulus)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", .stimulus, ".draw"))]

  draws <- epred_draws_metad(object, newdata, ..., .stimulus = .stimulus, .response = .response) |>
    group_by(.data$.row, !!!syms(.cols), .data$.chain, .data$.iteration, .data$.draw)

  if (by_correct) {
    draws |>
      mutate(correct = as.integer(!!sym(.stimulus) == !!sym(.response))) |>
      group_by(.data$correct, .add = TRUE) |>
      mutate(.epred = .data$.epred / sum(.data$.epred)) |> ## normalize within correct
      summarize(.epred = sum(.data$.epred * .data$confidence), .groups = "keep") |>
      group_by(.data$.row, !!!syms(.cols), .data$correct)
  } else {
    if (by_stimulus) {
      if (by_response) {
        draws |>
          group_by(!!sym(.stimulus), !!sym(.response), .add = TRUE) |>
          mutate(.epred = .data$.epred / sum(.data$.epred)) |> ## normalize within responses
          summarize(.epred = sum(.data$.epred * .data$confidence), .groups = "keep") |>
          group_by(.data$.row, !!!syms(.cols), !!sym(.stimulus), !!sym(.response))
      } else {
        draws |>
          group_by(!!sym(.stimulus), .add = TRUE) |>
          summarize(.epred = sum(.data$.epred * .data$confidence), .groups = "keep") |>
          group_by(.data$.row, !!!syms(.cols), !!sym(.stimulus))
      }
    } else {
      if (by_response) {
        draws |>
          group_by(!!sym(.response), .add = TRUE) |>
          mutate(.epred = .data$.epred / sum(.data$.epred)) |>
          group_by(
            .data$.row, !!!syms(.cols), .data$.chain, .data$.iteration, .data$.draw,
            !!sym(.response), .data$confidence
          ) |>
          mutate(.epred = .data$confidence * sum(.data$.epred)) |>
          group_by(
            .data$.row, !!!syms(.cols),
            .data$.chain, .data$.iteration, .data$.draw, !!sym(.response)
          ) |>
          summarize(.epred = sum(.data$.epred) / 2) |>
          group_by(.data$.row, !!!syms(.cols), !!sym(.response))
      } else {
        draws |>
          summarize(.epred = sum(.data$.epred * .data$confidence) / 2, .groups = "keep") |>
          group_by(.data$.row, !!!syms(.cols))
      }
    }
  }
}

#' @rdname mean_conf_draws
#' @export
add_mean_confidence_draws <- function(newdata, object, ...) {
  mean_confidence_draws(object, newdata, ...)
}

#' @rdname mean_conf_draws
#' @export
mean_confidence_rvars <- function(object, newdata, ...,
                                  .stimulus = "stimulus", .response = "response",
                                  by_stimulus = TRUE, by_response = TRUE,
                                  by_correct = FALSE) {
  ## grouping columns
  .stimulus <- get_stimulus(object, .default = .stimulus)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", .stimulus, ".draw"))]

  draws <- epred_rvars_metad(object, newdata, ..., .stimulus = .stimulus, .response = .response) |>
    group_by(.data$.row, !!!syms(.cols))

  if (by_correct) {
    draws |>
      mutate(correct = as.integer(!!sym(.stimulus) == !!sym(.response))) |>
      group_by(.data$correct, .add = TRUE) |>
      mutate(.epred = .data$.epred / posterior::rvar_sum(.data$.epred)) |> ## normalize within responses
      summarize(.epred = rvar_sum(.data$.epred * .data$confidence), .groups = "keep") |>
      group_by(.data$.row, !!!syms(.cols), .data$correct)
  } else {
    if (by_stimulus) {
      if (by_response) {
        draws |>
          group_by(!!sym(.stimulus), !!sym(.response), .add = TRUE) |>
          mutate(.epred = .data$.epred / posterior::rvar_sum(.data$.epred)) |> ## normalize within responses
          summarize(.epred = rvar_sum(.data$.epred * .data$confidence), .groups = "keep")
      } else {
        draws |>
          group_by(!!sym(.stimulus), .add = TRUE) |>
          summarize(.epred = rvar_sum(.data$.epred * .data$confidence), .groups = "keep")
      }
    } else {
      if (by_response) {
        draws |>
          group_by(!!sym(.response), .add = TRUE) |>
          mutate(.epred = .data$.epred / rvar_sum(.data$.epred)) |>
          group_by(.data$.row, !!!syms(.cols), !!sym(.response), .data$confidence) |>
          mutate(.epred = .data$confidence * rvar_sum(.data$.epred)) |>
          group_by(.data$.row, !!!syms(.cols), !!sym(.response)) |>
          summarize(.epred = rvar_sum(.data$.epred) / 2, .groups = "keep")
      } else {
        draws |>
          summarize(.epred = rvar_sum(.data$.epred * .data$confidence) / 2, .groups = "keep")
      }
    }
  }
}

#' @rdname mean_conf_draws
#' @export
add_mean_confidence_rvars <- function(newdata, object, ...) {
  mean_confidence_rvars(object, newdata, ...)
}

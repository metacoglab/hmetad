#' Obtain posterior draws of mean confidence
#'
#' @description Computes posterior mean confidence conditional on stimulus and
#'   response (\eqn{\mathbb{E}[C \;\vert\; S=s,R=r]}), stimulus (averaging over
#'   responses, \eqn{\mathbb{E}[C \;\vert\; S=s]}), response (averaging over
#'   stimuli, \eqn{\mathbb{E}[C \;\vert\; R=r]}), or neither (averaging over
#'   stimuli and responses, \eqn{\mathbb{E}[C]}). For `mean_confidence_draws`
#'   and `add_mean_confidence_draws`, estimates are returned in a tidy tibble
#'   with one row per posterior draw, stimulus, and response. For
#'   `mean_confidence_rvars` and `add_mean_confidence_rvars`, estimates are
#'   returned as [posterior::rvar]s, with one row per row in `newdata`.
#'
#'   `add_mean_confidence_draws` is an alias of `mean_confidence_draws` with
#'   argument order swapped.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional arguments to [tidybayes::epred_draws] or [tidybayes::epred_rvars]
#' @param by_stimulus If TRUE, predict mean confidence separately by stimulus.
#'   Otherwise, predict mean confidence averaging over stimuli.
#' @param by_response If TRUE, predict mean confidence separately by response
#'   Otherwise, predict mean confidence averaging over responses.
#' @returns a tibble containing posterior draws of mean confidence with the
#'   following columns:
#'   * `.row`: the row of `newdata`
#'   * `.chain`, `.iteration`, `.draw`: for `mean_confidence_draws` and `add_mean_confidence_draws`, identifiers for the posterior sample
#'   * `stimulus`: indicator for stimulus presence (if `by_stimulus==TRUE`)
#'   * `response`: indicator for type 1 response (if `by_response==TRUE`)
#'   * `.epred`: the predicted mean confidence
#' @rdname mean_conf_draws
#' @examples
#' # running few iterations so example runs quickly, use more in practice
#' m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # compute mean confidence by stimulus and response
#' mean_confidence_draws(m, newdata)
#' add_mean_confidence_draws(newdata, m)
#'
#' # compute mean confidence by stimulus
#' mean_confidence_draws(m, newdata, by_response = FALSE)
#'
#' # compute mean confidence by response
#' mean_confidence_draws(m, newdata, by_stimulus = FALSE)
#'
#' # compute mean confidence averaging over stimuli and responses
#' mean_confidence_draws(m, newdata, by_stimulus = FALSE, by_response = FALSE)
#' @export
mean_confidence_draws <- function(object, newdata, ...,
                                  by_stimulus = TRUE, by_response = TRUE) {
  ## grouping columns
  .stimulus <- get_stimulus(object)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", .stimulus, ".draw"))]

  draws <- epred_draws_metad(object, newdata, ...) |>
    group_by(.data$.row, !!!syms(.cols), .data$.chain, .data$.iteration, .data$.draw)

  if (by_stimulus) {
    if (by_response) {
      draws |>
        group_by(!!sym(.stimulus), .data$response, .add = TRUE) |>
        mutate(.epred = .data$.epred / sum(.data$.epred)) |> ## normalize within responses
        summarize(.epred = sum(.data$.epred * .data$confidence), .groups = "keep") |>
        group_by(.data$.row, !!!syms(.cols), !!sym(.stimulus), .data$response)
    } else {
      draws |>
        group_by(!!sym(.stimulus), .add = TRUE) |>
        summarize(.epred = sum(.data$.epred * .data$confidence), .groups = "keep") |>
        group_by(.data$.row, !!!syms(.cols), !!sym(.stimulus))
    }
  } else {
    if (by_response) {
      draws |>
        group_by(.data$response, .add = TRUE) |>
        mutate(.epred = .data$.epred / sum(.data$.epred)) |>
        group_by(
          .data$.row, !!!syms(.cols), .data$.chain, .data$.iteration, .data$.draw,
          .data$response, .data$confidence
        ) |>
        mutate(.epred = .data$confidence * sum(.data$.epred)) |>
        group_by(
          .data$.row, !!!syms(.cols),
          .data$.chain, .data$.iteration, .data$.draw, .data$response
        ) |>
        summarize(
          .epred = sum(.data$.epred) / 2 # , .groups = "keep"
        ) |>
        group_by(.data$.row, !!!syms(.cols), .data$response)
    } else {
      draws |>
        summarize(.epred = sum(.data$.epred * .data$confidence) / 2, .groups = "keep") |>
        group_by(.data$.row, !!!syms(.cols))
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
                                  by_stimulus = TRUE, by_response = TRUE) {
  ## grouping columns
  .stimulus <- get_stimulus(object)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", .stimulus, ".draw"))]

  draws <- epred_rvars_metad(object, newdata, ...) |>
    group_by(.row, !!!syms(.cols))

  if (by_stimulus) {
    if (by_response) {
      draws |>
        group_by(.data$stimulus, .data$response, .add = TRUE) |>
        mutate(.epred = .data$.epred / posterior::rvar_sum(.data$.epred)) |> ## normalize within responses
        summarize(.epred = rvar_sum(.data$.epred * .data$confidence), .groups = "keep")
    } else {
      draws |>
        group_by(.data$stimulus, .add = TRUE) |>
        summarize(.epred = rvar_sum(.data$.epred * .data$confidence), .groups = "keep")
    }
  } else {
    if (by_response) {
      draws |>
        group_by(.data$response, .add = TRUE) |>
        mutate(.epred = .data$.epred / rvar_sum(.data$.epred)) |>
        group_by(.data$.row, !!!syms(.cols), .data$response, .data$confidence) |>
        mutate(.epred = .data$confidence * rvar_sum(.data$.epred)) |>
        group_by(.data$.row, !!!syms(.cols), .data$response) |>
        summarize(.epred = rvar_sum(.data$.epred) / 2, .groups = "keep")
    } else {
      draws |>
        summarize(.epred = rvar_sum(.data$.epred * .data$confidence) / 2, .groups = "keep")
    }
  }
}

#' @rdname mean_conf_draws
#' @export
add_mean_confidence_rvars <- function(newdata, object, ...) {
  mean_confidence_rvars(object, newdata, ...)
}

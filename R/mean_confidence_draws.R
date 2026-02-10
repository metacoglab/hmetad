#' Obtain posterior draws of mean confidence
#'
#' @description
#' Computes posterior mean confidence conditional on stimulus and response
#' (\eqn{\mathbb{E}[C \;\vert\; S=s,R=r]}),
#' stimulus (averaging over responses, \eqn{\mathbb{E}[C \;\vert\; S=s]}),
#' response (averaging over stimuli, \eqn{\mathbb{E}[C \;\vert\; R=r]}),
#' or neither (averaging over stimuli and responses, \eqn{\mathbb{E}[C]}).
#'
#' `add_mean_confidence_draws` is an alias of `mean_confidence_draws` with
#' argument order swapped
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional arguments to [tidybayes::epred_draws]
#' @param by_stimulus If TRUE, predict mean confidence separately by stimulus.
#' Otherwise, predict mean confidence averaging over stimuli.
#' @param by_response If TRUE, predict mean confidence separately by response
#' Otherwise, predict mean confidence averaging over responses.
#' @returns a tibble containing posterior draws of mean confidence with the following
#' columns:
#'   * `.row`: the row of `newdata`
#'   * `.chain`, `.iteration`, `.draw`: identifiers for the posterior sample
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
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  draws <- tidybayes::epred_draws(object, newdata, ...)

  ## number of confidence levels
  K <- as.integer(n_distinct(draws$.category) / 4)

  ## grouping columns
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", "stimulus", ".draw"))]

  draws <- draws |>
    mutate(
      .category = as.integer(.data$.category),
      stimulus = as.integer(.data$.category > 2 * K),
      joint_response = ifelse(.data$stimulus,
        .data$.category - 2 * K,
        .data$.category
      ),
      response = type1_response(.data$joint_response, K),
      confidence = type2_response(.data$joint_response, K)
    )

  if (by_stimulus) {
    if (by_response) {
      draws |>
        group_by(
          .data$.row, .data$.chain, .data$.iteration, .data$.draw,
          .data$stimulus, .data$response, !!!syms(.cols)
        ) |>
        mutate(.epred = .data$.epred / sum(.data$.epred)) |> ## normalize within responses
        summarize(.epred = sum(.data$.epred * .data$confidence), .groups = "keep") |>
        group_by(.data$.row, .data$stimulus, .data$response, !!!syms(.cols))
    } else {
      draws |>
        group_by(
          .data$.row, .data$.chain, .data$.iteration, .data$.draw,
          .data$stimulus, !!!syms(.cols)
        ) |>
        summarize(.epred = sum(.data$.epred * .data$confidence), .groups = "keep") |>
        group_by(.data$.row, .data$stimulus, !!!syms(.cols))
    }
  } else {
    if (by_response) {
      draws |>
        group_by(
          .data$.row, .data$.chain, .data$.iteration, .data$.draw,
          !!!syms(.cols), .data$response
        ) |>
        mutate(.epred = .data$.epred / sum(.data$.epred)) |>
        group_by(
          .data$.row, .data$.chain, .data$.iteration, .data$.draw,
          !!!syms(.cols), .data$response, .data$confidence
        ) |>
        mutate(.epred = .data$confidence * sum(.data$.epred)) |>
        group_by(
          .data$.row, .data$.chain, .data$.iteration, .data$.draw,
          !!!syms(.cols), .data$response
        ) |>
        summarize(.epred = sum(.data$.epred) / 2, .groups = "keep") |>
        group_by(.data$.row, .data$response, !!!syms(.cols))
    } else {
      draws |>
        group_by(
          .data$.row, .data$.chain, .data$.iteration, .data$.draw,
          !!!syms(.cols)
        ) |>
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

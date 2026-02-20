#' Obtain posterior predictions of joint responses
#'
#' @description Given a data frame and a meta-d' model, adds predictions of joint
#'   type 1 and type 2 responses For `predicted_draws_metad` and
#'   `add_predicted_draws_metad`, predictions are returned in a tidy tibble with one
#'   row per posterior draw. For `predicted_rvars_metad` and
#'   `add_predicted_rvars_metad`, parameters are returned as [posterior::rvar]s,
#'   with one row per row in `newdata`.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional arguments passed to [tidybayes::add_predicted_draws] or
#'   [tidybayes::add_predicted_rvars]
#' @returns a tibble containing posterior draws of model parameters with the
#'   following columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: for `predicted_draws_metad`, identifiers for the posterior sample
#'  * `stimulus`, `joint_response`, `response`, `confidence`: identifiers for the response type
#'  * `.prediction`: predicted type 1 and type 2 responses given the stimulus
#' @examples
#' # running few iterations so example runs quickly, use more in practice
#' m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#'
#' # obtain model predictions
#' predicted_draws_metad(m, m$data)
#' add_predicted_draws_metad(m$data, m)
#'
#' # obtain model predictions (posterior::rvar)
#' predicted_rvars_metad(m, m$data)
#' add_predicted_rvars_metad(m$data, m)
#'
#' @rdname predicted_draws_metad
#' @export
predicted_draws_metad <- function(object, newdata, ...) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  ## grouping columns
  .stimulus <- get_stimulus(object)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, "joint_response",
    "response", "confidence"
  ))]

  if (get_ll(object) == "multinomial") {
    draws <- tidybayes::predicted_draws(object, newdata, ...) |>
      tidyr::separate_wider_delim(
        ".category",
        delim = "_",
        names = c(NA, "stimulus", "joint_response")
      ) |>
      mutate(
        stimulus = as.integer(.data$stimulus),
        joint_response = as.integer(.data$joint_response),
      ) |>
      relocate(".prediction") |>
      ungroup()

    ## number of confidence levels
    K <- as.integer(n_distinct(draws$joint_response) / 2)

    draws <- draws |>
      mutate(
        response = type1_response(.data$joint_response, K),
        confidence = type2_response(.data$joint_response, K)
      ) |>
      relocate(
        ".row", !!!syms(.cols), "stimulus", "joint_response",
        "response", "confidence"
      ) |>
      group_by(
        .row, !!!syms(.cols), .data$stimulus, .data$joint_response,
        .data$response, .data$confidence
      )
  } else {
    if (.stimulus %in% names(newdata)) {
      draws <- tidybayes::predicted_draws(object, newdata, ...)
    } else {
      ## get predicted_draws separately by stimulus
      draws <- newdata |>
        mutate("{.stimulus}" := 0L) |>
        tidybayes::add_predicted_draws(object, ...) |>
        bind_rows(newdata |> mutate("{.stimulus}" := 1L) |>
          tidybayes::add_predicted_draws(object, ...))
    }

    ## number of confidence levels
    K <- as.integer(n_distinct(draws$.prediction) / 2)

    draws |>
      rename(joint_response = .data$.prediction) |>
      mutate(joint_response = as.integer(.data$joint_response)) |>
      ungroup() |>
      mutate(
        response = type1_response(.data$joint_response, K),
        confidence = type2_response(.data$joint_response, K)
      ) |>
      relocate(
        ".row", !!!syms(.cols), !!sym(.stimulus), "joint_response",
        "response", "confidence"
      ) |>
      group_by(.row, !!!syms(.cols), !!sym(.stimulus))
  }
}

#' @rdname predicted_draws_metad
#' @export
add_predicted_draws_metad <- function(newdata, object, ...) {
  predicted_draws_metad(object, newdata, ...)
}

#' @rdname predicted_draws_metad
#' @export
predicted_rvars_metad <- function(object, newdata, ...) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  ## grouping columns
  .stimulus <- get_stimulus(object)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, "joint_response",
    "response", "confidence"
  ))]

  draws <- NULL
  if (get_ll(object) == "multinomial") {
    draws <- tidybayes::predicted_rvars(object, newdata,
      ...,
      columns_to = ".category"
    ) |>
      tidyr::separate_wider_delim(
        ".category",
        delim = "_",
        names = c(NA, "stimulus", "joint_response")
      ) |>
      mutate(
        stimulus = as.integer(.data$stimulus),
        joint_response = as.integer(.data$joint_response),
      ) |>
      relocate(".prediction")

    ## number of confidence levels
    K <- as.integer(n_distinct(draws$joint_response) / 2)

    draws <- draws |>
      mutate(
        response = type1_response(.data$joint_response, K),
        confidence = type2_response(.data$joint_response, K)
      ) |>
      relocate(
        ".row", !!!syms(.cols), "stimulus", "joint_response",
        "response", "confidence"
      ) |>
      group_by(
        .data$.row, !!!syms(.cols), .data$stimulus, .data$joint_response,
        .data$response, .data$confidence
      )
  } else {
    if (.stimulus %in% names(newdata)) {
      draws <- tidybayes::predicted_draws(object, newdata, ...)
    } else {
      ## get predicted_draws separately by stimulus
      draws <- newdata |>
        mutate("{.stimulus}" := 0L) |>
        tidybayes::add_predicted_rvars(object, ...) |>
        bind_rows(newdata |> mutate("{.stimulus}" := 1L) |>
          tidybayes::add_predicted_rvars(object, ...))
    }

    ## number of confidence levels
    K <- as.integer(max(draws$.prediction)[1] / 2)

    draws <- draws |>
      rename(joint_response = .data$.prediction) |>
      mutate(
        response = joint_response > K,
        confidence = rvar_ifelse(
          joint_response > K,
          joint_response - K,
          K + 1 - joint_response
        )
      ) |>
      relocate(".row", !!!syms(.cols), !!sym(.stimulus)) |>
      group_by(.data$.row, !!!syms(.cols), !!sym(.stimulus))
  }

  draws
}

#' @rdname predicted_draws_metad
#' @export
add_predicted_rvars_metad <- function(newdata, object, ...) {
  predicted_rvars_metad(object, newdata, ...)
}

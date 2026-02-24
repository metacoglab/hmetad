#' Obtain posterior draws of joint response probabilities
#'
#' @description Given a data frame and a meta-d' model, adds estimates of joint
#'   type 1 and type 2 response probabilities. For `epred_draws_metad` and
#'   `add_epred_draws_metad`, estimates are returned in a tidy tibble with one
#'   row per posterior draw. For `epred_rvars_metad` and
#'   `add_epred_rvars_metad`, parameters are returned as [posterior::rvar]s,
#'   with one row per row in `newdata`.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional arguments passed to [tidybayes::add_epred_draws] or
#'   [tidybayes::add_epred_rvars]
#' @returns a tibble containing posterior draws of model parameters with the
#'   following columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: for `epred_draws_metad`, identifiers for the posterior sample
#'  * `stimulus`, `joint_response`, `response`, `confidence`: identifiers for the response type
#'  * `.epred`: probability of the type 1 and type 2 response given the stimulus, \eqn{P(R, C \;\vert\; S)}
#' @examples
#' \dontrun{
#' # running few iterations so example runs quickly, use more in practice
#' example_data <- sim_metad(N_trials = 1000)
#' example_model <- fit_metad(N ~ 1, example_data, chains = 1, iter = 500)
#' }
#' example_model <- hmetad:::example_model
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # obtain model predictions
#' epred_draws_metad(example_model, newdata)
#' \dontrun{
#' add_epred_draws_metad(newdata, example_model)
#' }
#'
#' # obtain model predictions (posterior::rvar)
#' epred_rvars_metad(example_model, newdata)
#' \dontrun{
#' add_epred_rvars_metad(newdata, example_model)
#' }
#'
#' @rdname epred_draws_metad
#' @export
epred_draws_metad <- function(object, newdata, ...) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  ## recover types of independent variables
  object <- tidybayes::recover_types(object)

  ## grouping columns
  .stimulus <- get_stimulus(object)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, "joint_response",
    "response", "confidence"
  ))]

  draws <- NULL
  if (get_ll(object) == "multinomial") {
    .stimulus <- "stimulus"
    draws <- tidybayes::epred_draws(object, newdata, ...) |>
      tidyr::separate_wider_delim(
        ".category",
        delim = "_",
        names = c(NA, "stimulus", "joint_response")
      ) |>
      mutate(
        stimulus = as.integer(.data$stimulus),
        joint_response = as.integer(.data$joint_response),
      )
  } else {
    if (.stimulus %in% names(newdata)) {
      draws <- tidybayes::epred_draws(object, newdata, ...)
    } else {
      ## get epred_draws separately by stimulus
      draws <- newdata |>
        mutate("{.stimulus}" := 0L) |>
        tidybayes::add_epred_draws(object, ...) |>
        bind_rows(newdata |> mutate("{.stimulus}" := 1L) |>
          tidybayes::add_epred_draws(object, ...))
    }

    draws <- draws |>
      rename(joint_response = .data$.category) |>
      mutate(joint_response = as.integer(.data$joint_response))
  }

  ## number of confidence levels
  K <- as.integer(n_distinct(draws$joint_response) / 2)

  ## determine type 1/type 2 responses
  draws |>
    mutate(
      response = type1_response(.data$joint_response, K),
      confidence = type2_response(.data$joint_response, K)
    ) |>
    relocate(
      ".row", !!!syms(.cols), !!sym(.stimulus), "joint_response",
      "response", "confidence", ".epred"
    ) |>
    group_by(
      .row, !!!syms(.cols), !!sym(.stimulus), .data$joint_response,
      .data$response, .data$confidence
    )
}

#' @rdname epred_draws_metad
#' @export
add_epred_draws_metad <- function(newdata, object, ...) {
  epred_draws_metad(object, newdata, ...)
}

#' @rdname epred_draws_metad
#' @export
epred_rvars_metad <- function(object, newdata, ...) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  ## recover types of independent variables
  object <- tidybayes::recover_types(object)

  ## grouping columns
  .stimulus <- get_stimulus(object)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, "joint_response",
    "response", "confidence"
  ))]

  draws <- NULL
  if (get_ll(object) == "multinomial") {
    .stimulus <- "stimulus"
    draws <- tidybayes::epred_rvars(object, newdata,
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
      )
  } else {
    if (.stimulus %in% names(newdata)) {
      draws <- tidybayes::epred_rvars(object, newdata, ..., columns_to = ".category")
    } else {
      ## get epred_rvars separately by stimulus
      draws <- newdata |>
        mutate("{.stimulus}" := 0L) |>
        tidybayes::add_epred_rvars(object, ..., columns_to = ".category") |>
        bind_rows(newdata |> mutate("{.stimulus}" := 1L) |>
          tidybayes::add_epred_rvars(object, ..., columns_to = ".category"))
    }

    draws <- draws |>
      rename(joint_response = .data$.category) |>
      mutate(joint_response = as.integer(.data$joint_response))
  }

  ## number of confidence levels
  K <- as.integer(n_distinct(draws$joint_response) / 2)

  draws |>
    mutate(
      response = type1_response(.data$joint_response, K),
      confidence = type2_response(.data$joint_response, K)
    ) |>
    relocate(
      ".row", !!!syms(.cols), !!sym(.stimulus), "joint_response",
      "response", "confidence", ".epred"
    ) |>
    group_by(
      .row, !!!syms(.cols), !!sym(.stimulus), .data$joint_response,
      .data$response, .data$confidence
    )
}

#' @rdname epred_draws_metad
#' @export
add_epred_rvars_metad <- function(newdata, object, ...) {
  epred_rvars_metad(object, newdata, ...)
}

#' Calculate empirical joint type 1/type 2 response probabilities
#'
#' Given a dataset `data`, determine the probability of each combination of type
#' 1 and type 2 responses, optionally conditional on stimulus.
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
#' @param by_stimulus If `TRUE` (default), calculate type 2 response
#'   probabilities conditional on stimulus.
#' @returns A tibble with columns:
#'  * `...`: the grouping columns in `data`
#'  * `{.stimulus}` (if `by_stimulus=TRUE`): the stimulus
#'  * `{.response}`: the type 1 response
#'  * `{.confidence}`: the type 2 response
#'  * `{.joint_response}`: the joint type 1/type 2 response
#'  * `n`: the number of rows in `data` with the corresponding `stimulus` (if `by_stimulus=TRUE`), `response`, `confidence`, and `joint_response`
#'  * `p`: the proportion of rows in `data` with the corresponding `response` (per `stimulus` if `by_stimulus=TRUE`)
#' @examples
#' # calculate type 2 response probabilities by stimulus
#' joint_probabilities(example_data())
#'
#' # calculate type 2 response probabilities by condition, averaging over stimuli
#' joint_probabilities(sim_metad_condition(), condition, by_stimulus = FALSE)
#' @seealso [epred_draws()], [epred_rvars()]
#' @export
joint_probabilities <- function(
  data, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", .joint_response = "joint_response",
  K = NULL, by_stimulus = TRUE
) {
  data <- .aggregate_metad(
    data, ...,
    .stimulus = .stimulus, .response = .response,
    .confidence = .confidence, .joint_response = .joint_response, K = K
  ) |>
    group_by(...) |>
    arrange(..., !!sym(.stimulus), !!sym(.joint_response))

  if (by_stimulus) {
    data <- data |> group_by(!!sym(.stimulus), .add = TRUE)
  } else {
    data <- data |>
      group_by(!!sym(.joint_response), !!sym(.response), !!sym(.confidence), .add = TRUE) |>
      summarize(n = sum(.data$n)) |>
      relocate(!!sym(.joint_response), .before = "n") |>
      group_by(...)
  }

  data |>
    mutate(p = .data$n / sum(.data$n))
}


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
#' @param .stimulus The name of "stimulus" column
#' @param .response The name of "response" column
#' @param .confidence The name of "confidence" column
#' @param .joint_response The name of "joint_response" column
#' @returns a tibble containing posterior draws of model parameters with the
#'   following columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: for `epred_draws_metad`, identifiers for the posterior sample
#'  * `{.stimulus}`, `{.joint_response}`, `{.response}`, `{.confidence}`: identifiers for the response type
#'  * `.epred`: probability of the type 1 and type 2 response given the stimulus, \eqn{P(R, C \;\vert\; S)}
#' @examples
#' \donttest{
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # obtain model predictions
#' # equivalent to `add_epred_draws_metad(newdata, example_model())`
#' epred_draws_metad(example_model(), newdata)
#'
#' # obtain model predictions (`posterior::rvar`)
#' # equivalent to `add_epred_rvars_metad(newdata, example_model())`
#' epred_rvars_metad(example_model(), newdata)
#' }
#' @rdname epred_draws_metad
#' @seealso [joint_probabilities()], [tidybayes::epred_draws()],
#'   [tidybayes::epred_rvars()]
#' @export
epred_draws_metad <- function(
  object, newdata, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", .joint_response = "joint_response"
) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  ## recover types of independent variables
  object <- tidybayes::recover_types(object)

  ## grouping columns
  .stimulus <- get_stimulus(object, .default = .stimulus)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, .joint_response,
    .response, .confidence
  ))]

  draws <- NULL
  if (get_ll(object) == "multinomial") {
    draws <- tidybayes::epred_draws(object, newdata, ...) |>
      tidyr::separate_wider_delim(
        ".category",
        delim = "_",
        names = c(NA, .stimulus, .joint_response)
      ) |>
      mutate(
        "{.stimulus}" := as.integer(!!sym(.stimulus)),
        "{.joint_response}" := as.integer(!!sym(.joint_response)),
      )
  } else {
    if (.stimulus %in% names(newdata)) {
      draws <- tidybayes::epred_draws(object, newdata, ...)
    } else {
      ## get epred_draws separately by stimulus
      draws <- newdata |>
        mutate("{.stimulus}" := 0L) |>
        tidybayes::add_epred_draws(object, ...) |>
        bind_rows(newdata |>
          mutate("{.stimulus}" := 1L) |>
          tidybayes::add_epred_draws(object, ...))
    }

    draws <- draws |>
      rename("{.joint_response}" := .data$.category) |>
      mutate("{.joint_response}" := as.integer(!!sym(.joint_response)))
  }

  ## number of confidence levels
  K <- as.integer(n_distinct(pull(draws, .joint_response)) / 2)

  ## determine type 1/type 2 responses
  draws |>
    mutate(
      "{.response}" := type1_response(!!sym(.joint_response), K),
      "{.confidence}" := type2_response(!!sym(.joint_response), K)
    ) |>
    relocate(
      ".row", !!!syms(.cols), !!sym(.stimulus), !!sym(.joint_response),
      !!sym(.response), !!sym(.confidence), ".epred"
    ) |>
    group_by(
      .data$.row, !!!syms(.cols), !!sym(.stimulus), !!sym(.joint_response),
      !!sym(.response), !!sym(.confidence)
    )
}

#' @rdname epred_draws_metad
#' @export
add_epred_draws_metad <- function(newdata, object, ...) {
  epred_draws_metad(object, newdata, ...)
}

#' @rdname epred_draws_metad
#' @export
epred_rvars_metad <- function(
  object, newdata, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", .joint_response = "joint_response"
) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  ## recover types of independent variables
  object <- tidybayes::recover_types(object)

  ## grouping columns
  .stimulus <- get_stimulus(object, .default = .stimulus)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(
    ".row", .stimulus, .joint_response,
    .response, .confidence
  ))]

  draws <- NULL
  if (get_ll(object) == "multinomial") {
    draws <- tidybayes::epred_rvars(object, newdata,
      ...,
      columns_to = ".category"
    ) |>
      tidyr::separate_wider_delim(
        ".category",
        delim = "_",
        names = c(NA, .stimulus, .joint_response)
      ) |>
      mutate(
        "{.stimulus}" := as.integer(!!sym(.stimulus)),
        "{.joint_response}" := as.integer(!!sym(.joint_response)),
      )
  } else {
    if (.stimulus %in% names(newdata)) {
      draws <- tidybayes::epred_rvars(object, newdata, ..., columns_to = ".category")
    } else {
      ## get epred_rvars separately by stimulus
      draws <- newdata |>
        mutate("{.stimulus}" := 0L) |>
        tidybayes::add_epred_rvars(object, ..., columns_to = ".category") |>
        bind_rows(newdata |>
          mutate("{.stimulus}" := 1L) |>
          tidybayes::add_epred_rvars(object, ..., columns_to = ".category"))
    }

    draws <- draws |>
      rename("{.joint_response}" := .data$.category) |>
      mutate("{.joint_response}" := as.integer(!!sym(.joint_response)))
  }

  ## number of confidence levels
  K <- as.integer(n_distinct(pull(draws, .joint_response)) / 2)

  draws |>
    mutate(
      "{.response}" := type1_response(!!sym(.joint_response), K),
      "{.confidence}" := type2_response(!!sym(.joint_response), K)
    ) |>
    relocate(
      ".row", !!!syms(.cols), !!sym(.stimulus), !!sym(.joint_response),
      !!sym(.response), !!sym(.confidence), ".epred"
    ) |>
    group_by(
      .data$.row, !!!syms(.cols), !!sym(.stimulus), !!sym(.joint_response),
      !!sym(.response), !!sym(.confidence)
    )
}

#' @rdname epred_draws_metad
#' @export
add_epred_rvars_metad <- function(newdata, object, ...) {
  epred_rvars_metad(object, newdata, ...)
}

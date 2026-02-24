#' Obtain posterior draws of meta-d' model parameters
#'
#' @description Given a data frame and a meta-d' model, adds estimates of all
#'   model parameters. For `linpred_draws_metad` and `add_linpred_draws_metad`, parameters are
#'   returned in a tidy tibble with one row per posterior draw. For
#'   `linpred_rvars_metad` and `add_linpred_rvars_metad`, parameters are returned as
#'   [posterior::rvar]s, with one row per row in `newdata`.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional arguments passed to [tidybayes::add_linpred_draws] or
#'   [tidybayes::add_linpred_rvars]
#' @param pivot_longer Return the draws in long format?
#' * if `TRUE`, resulting data frame has one row per posterior draw per model parameter
#' * if `FALSE` (default), resulting data frame has one row per posterior draw
#' @returns a tibble containing posterior draws of model parameters with the
#'   following columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: for `linpred_draws_metad`, identifiers for the posterior sample
#'  * `.variable`, `.value`: if `pivot_longer=TRUE`, `.variable` identifies different meta-d' model parameters and `.value` stores posterior samples
#'  * `M`, `dprime`, `c`, `meta_dprime`, `meta_c`, `meta_c2_0_<k>`, `meta_c2_1_<k>`: if `pivot_longer=FALSE`, posterior samples of all meta-d' model parameters
#' @examples
#' \dontrun{
#' # running few iterations so example runs quickly, use more in practice
#' example_data <- sim_metad(N_trials = 1000)
#' example_model <- fit_metad(N ~ 1, example_data, chains = 1, iter = 500)
#' }
#' example_model <- hmetad:::example_model
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # obtain model parameters (wide format)
#' linpred_draws_metad(example_model, newdata)
#' add_linpred_draws_metad(newdata, example_model)
#'
#' # obtain model parameters (long format)
#' linpred_draws_metad(example_model, newdata, pivot_longer = TRUE)
#' add_linpred_draws_metad(newdata, example_model, pivot_longer = TRUE)
#'
#' # obtain model parameters (wide format, posterior::rvar)
#' linpred_rvars_metad(example_model, newdata)
#' add_linpred_rvars_metad(newdata, example_model)
#'
#' # obtain model parameters (long format, posterior::rvar)
#' linpred_rvars_metad(example_model, newdata, pivot_longer = TRUE)
#' add_linpred_rvars_metad(newdata, example_model, pivot_longer = TRUE)
#'
#' @rdname linpred_draws_metad
#' @export
linpred_draws_metad <- function(object, newdata, ..., pivot_longer = FALSE) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  ## grouping columns
  .stimulus <- get_stimulus(object)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", .stimulus, ".draw"))]

  ## set stimulus for categorical models (not used in linpred_draws)
  if (get_ll(object) == "categorical") {
    newdata <- newdata |> mutate("{.stimulus}" := 0L)
  }

  draws <- object |>
    tidybayes::linpred_draws(newdata, ..., value = "M", dpar = TRUE, transform = TRUE) |>
    select(-"mu") |>
    mutate(
      meta_dprime = .data$M * .data$dprime,
      meta_c = ifelse(get_metac(object) == "absolute",
        .data$c,
        .data$M * .data$c
      )
    ) |>
    tidyr::pivot_longer(starts_with("metac2"),
      names_pattern = "metac2(zero|one)([[:digit:]*])diff",
      names_to = c("response", "k"),
      values_to = "diff"
    ) |>
    mutate(response = as.numeric(.data$response == "one")) |>
    group_by(
      .data$.row, !!!syms(.cols),
      .data$.chain, .data$.iteration, .data$.draw, .data$response
    ) |>
    mutate(c2 = ifelse(.data$response, .data$meta_c + cumsum(.data$diff),
      .data$meta_c - cumsum(.data$diff)
    )) |>
    ungroup() |>
    select(-"diff") |>
    tidyr::pivot_wider(
      names_from = c("response", "k"), values_from = "c2",
      names_prefix = "meta_c2_"
    )

  ## remove stimulus column for categorical models
  if (get_ll(object) == "categorical") {
    draws <- draws |> select(-!!sym(.stimulus))
  }

  if (pivot_longer) {
    draws |>
      tidyr::pivot_longer(
        c(
          "M", "dprime", "c", "meta_dprime",
          "meta_c", starts_with("meta_c2")
        ),
        names_to = ".variable", values_to = ".value"
      ) |>
      group_by(.data$.row, !!!syms(.cols), .data$.variable)
  } else {
    draws |>
      group_by(.data$.row, !!!syms(.cols))
  }
}

#' @rdname linpred_draws_metad
#' @export
add_linpred_draws_metad <- function(newdata, object, ..., pivot_longer = FALSE) {
  linpred_draws_metad(object, newdata, ..., pivot_longer = pivot_longer)
}

#' @rdname linpred_draws_metad
#' @export
linpred_rvars_metad <- function(object, newdata, ..., pivot_longer = FALSE) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  ## grouping columns
  .stimulus <- get_stimulus(object)
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", .stimulus, ".draw"))]

  ## set stimulus for categorical models (not used in linpred_draws)
  if (get_ll(object) == "categorical") {
    newdata <- newdata |> mutate("{.stimulus}" := 0L)
  }

  draws <- object |>
    tidybayes::linpred_rvars(newdata, ..., value = "M", dpar = TRUE, transform = TRUE) |>
    select(-"mu") |>
    mutate(
      meta_dprime = .data$M * .data$dprime,
      meta_c = .data$c
    ) |>
    tidyr::pivot_longer(starts_with("metac2"),
      names_pattern = "metac2(zero|one)([[:digit:]*])diff",
      names_to = c("response", "k"),
      values_to = "diff"
    ) |>
    mutate(response = as.numeric(.data$response == "one")) |>
    group_by(
      .data$.row, ## !!!syms(.cols),
      .data$response
    ) |>
    mutate(c2 = posterior::rvar_ifelse(
      .data$response == 1,
      .data$meta_c + cumsum(.data$diff),
      .data$meta_c - cumsum(.data$diff)
    )) |>
    ungroup() |>
    select(-"diff") |>
    tidyr::pivot_wider(
      names_from = c("response", "k"), values_from = "c2",
      names_prefix = "meta_c2_"
    )

  ## remove stimulus column for categorical models
  if (get_ll(object) == "categorical") {
    draws <- draws |> select(-!!sym(.stimulus))
  }

  if (pivot_longer) {
    draws |>
      tidyr::pivot_longer(
        c(
          "M", "dprime", "c", "meta_dprime",
          "meta_c", starts_with("meta_c2")
        ),
        names_to = ".variable", values_to = ".value"
      ) |>
      group_by(.data$.row, !!!syms(.cols), .data$.variable)
  } else {
    draws |>
      group_by(.data$.row, !!!syms(.cols))
  }
}

#' @rdname linpred_draws_metad
#' @export
add_linpred_rvars_metad <- function(newdata, object, pivot_longer = FALSE) {
  linpred_rvars_metad(object, newdata, pivot_longer = pivot_longer)
}

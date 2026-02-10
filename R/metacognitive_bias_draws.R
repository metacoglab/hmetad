#' Given the distances between successive confidence thresholds,
#' calculate the average of the cumulative distances to 0.
#' @param ... a series of distances between confidence thresholds
#' @keywords internal
metacognitive_bias <- function(...) {
  k <- length(c(...))
  sum(c(...) * (k:1) / k)
}

#' Obtain posterior draws of an index of metacognitive bias
#'
#' @description
#' Computes \eqn{\textrm{meta-}\Delta}, an index of metacognitive bias.
#' \eqn{\textrm{meta-}\Delta} is the distance between `meta_c` and the
#' average of the the confidence criteria `meta_c2_0` and `meta_c2_1`.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional parameters passed to [tidybayes::epred_draws]
#' @param by_response If `TRUE`, compute metacognitive bias separately for the
#' two type 1 responses. If `FALSE`, compute an un-weighted average of the two measures.
#' @returns a tibble containing posterior draws of  with the following
#' columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: identifiers for the posterior sample
#'  * `response`: the type 1 response for perceived stimulus presence
#'  * `metacognitive_bias`: the distance between `meta_c` and the average of
#'  the confidence criteria `meta_c2_{response}`.
#' @rdname bias_draws
#' @examples
#' # running few iterations so example runs quickly, use more in practice
#' m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # compute pseudo-type 1 ROC curve
#' metacognitive_bias_draws(m, newdata)
#' add_metacognitive_bias_draws(newdata, m)
#'
#' # average over the two type 1 responses
#' metacognitive_bias_draws(m, newdata, by_response = FALSE)
#' @export
metacognitive_bias_draws <- function(object, newdata, ..., by_response = TRUE) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  dpar <- object$family$dpar[stringr::str_starts(object$family$dpar, "metac2")]
  draws <- tidybayes::linpred_draws(object, newdata, ..., dpar = dpar, transform = TRUE)

  ## grouping columns
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", ".draw"))]

  draws <- draws |>
    group_by(!!!.cols) |>
    select(".draw", !!!.cols, starts_with("metac2")) |>
    tidyr::pivot_longer(starts_with("metac2"),
      names_to = c("response", "confidence"),
      names_pattern = "metac2([[:alpha:]]*)([[:digit:]])diff"
    ) |>
    mutate(response = as.integer(.data$response == "one")) |>
    group_by(!!!.cols, .data$response, .data$.draw) |>
    summarize(metacognitive_bias = metacognitive_bias(.data$value))

  if (!by_response) {
    draws <- draws |>
      group_by(!!!.cols, .data$.draw) |>
      summarize(metacognitive_bias = mean(.data$metacognitive_bias))
  }

  draws
}

#' @rdname bias_draws
#' @export
add_metacognitive_bias_draws <- function(newdata, object, ...) {
  metacognitive_bias_draws(object, newdata, ...)
}

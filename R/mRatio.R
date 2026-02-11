#' Convert binary variable \eqn{x} between \eqn{\{0, 1\}} and \eqn{\{-1, 1\}}
#' @description
#'  * `to_signed(x)` converts \eqn{x \in \{0, 1\}} to \eqn{x' \in \{-1, 1\}}
#'  * `to_unsigned(x)` converts \eqn{x \in \{-1, 1\}} to \eqn{x' \in \{0, 1\}}
#' @param x A binary variable
#' @returns A signed (for `to_signed`) or unsigned (for `to_unsigned`) version
#'   of `x`
#' @examples
#' # should return `1`
#' to_signed(0)
#'
#' # should return `1`
#' to_signed(1)
#'
#' # should return `0`
#' to_unsigned(-1)
#'
#' # should return `1`
#' to_unsigned(1)
#'
#' # `to_signed` also works with objects `R` interprets as `0` or `1`
#' to_signed(10)
#'
#' # `to_unsigned` also works with any signed integer
#' to_unsigned(-10)
#'
#' # neither function works with factors
#' to_unsigned(factor(1))
#' @rdname signed
#' @export
to_signed <- function(x) ifelse(x, 1, -1)

#' @rdname signed
#' @export
to_unsigned <- function(x) as.numeric(x > 0)

#' Convert between separate and joint type 1/type 2 responses
#'
#' @description
#' Confidence ratings and decisions are collected in one of two ways.
#'  * For separate ratings, there will be a type 1 response (\eqn{R \in \{0, 1\}}) and a
#' type 2 response (\eqn{C \in [1, K]}).
#'  * For joint ratings, there is instead a combined type 1/type 2 response
#' (\eqn{J \in [1, 2K]}), with values in \eqn{[1, K]} indicating a type 1 response of \eqn{0}
#' and values in \eqn{[K+1, 2K]} indicating a type 1 response of \eqn{1}, with
#' confident responses at the ends of the scale.
#'
#' `joint_response` converts separate type 1 and type 2 responses into the joint
#' format
#'
#' `type1_response` and `type2_response` convert the joint response into separate
#' responses.
#'
#' @param joint_response A joint type 1/type 2 response
#' @param response A type 1 response (`0` or `1`)
#' @param confidence A type 2 response/confidence rating (in `1:K`)
#' @param K The number of confidence levels
#' @rdname responses
#' @examples
#' # convert joint_response to separate responses
#' joint <- 1:8
#' K <- 4
#' type1_response(joint, K)
#' type2_response(joint, K)
#'
#' # convert separate responses to a joint response
#' t1 <- rep(c(0, 1), each = 4)
#' t2 <- c(4:1, 1:4)
#' joint_response(t1, t2, K)
#'
#' @export
joint_response <- function(response, confidence, K) {
  ifelse(response, confidence + K, K + 1 - confidence)
}

#' @rdname responses
#' @export
type1_response <- function(joint_response, K) {
  as.integer(joint_response > K)
}

#' @rdname responses
#' @export
type2_response <- function(joint_response, K) {
  ifelse(joint_response > K,
    joint_response - K,
    K + 1 - joint_response
  )
}

#' Compute joint response probabilities from aggregated counts
#'
#' @param counts A vector (or matrix) of counts of joint type 1/type 2
#' responses as provided by [aggregate_metad]
#' @returns A vector (or matrix) of response probabilities \eqn{P(R, C \;\vert\; S)}
#' @details
#' For response \eqn{R}, confidence \eqn{C}, stimulus \eqn{S}, and number of
#' confidence levels \eqn{K}, `counts` should be a vector (or matrix with rows)
#' of the form:
#' \deqn{
#'  [N_{S=0, R=0, C=K}, \ldots, N_{S=0, R=0, C=1}, \\
#'  N_{S=0, R=1, C=1}, \ldots, N_{S=0, R=1, C=K}, \\
#'  N_{S=1, R=0, C=K}, \ldots, N_{S=1, R=0, C=1}, \\
#'  N_{S=1, R=1, C=1}, \ldots, N_{S=1, R=1, C=K}] \\
#' }{
#' [N(R=0, C=K, S=0), ..., N(R=0, C=1, S=0),
#'  N(R=1, C=1, S=0), ..., N(R=1, C=K, S=0),
#'  N(R=0, C=K, S=1), ..., N(R=0, C=1, S=1),
#'  N(R=1, C=1, S=1), ..., N(R=1, C=K, S=1)]
#' }
#'
#'
#' Returns a vector (or matrix with rows) of the form:
#' \deqn{
#' [P(R=0, C=K \;\vert\; S=0), ..., P(R=0, C=1 \;\vert\; S=0), \\
#'  P(R=1, C=1 \;\vert\; S=0), ..., P(R=1, C=K \;\vert\; S=0), \\
#'  P(R=0, C=K \;\vert\; S=1), ..., P(R=0, C=1 \;\vert\; S=1), \\
#'  P(R=1, C=1 \;\vert\; S=1), ..., P(R=1, C=K \;\vert\; S=1)]
#' }{
#' [P(R=0, C=K | S=0), ..., P(R=0, C=1 | S=0),
#'  P(R=1, C=1 | S=0), ..., P(R=1, C=K | S=0),
#'  P(R=0, C=K | S=1), ..., P(R=0, C=1 | S=1),
#'  P(R=1, C=1 | S=1), ..., P(R=1, C=K | S=1)]
#' }
#' @examples
#' # Aggregate responses from simulated data
#' d <- sim_metad() |> aggregate_metad()
#'
#' # Compute conditional response probabilities
#' response_probabilities(d$N)
#'
#' # Also works on matrices
#' matrix(rep(1, 16), nrow = 2) |> response_probabilities()
#' @export
response_probabilities <- function(counts) {
  if (is.vector(counts)) {
    if ((length(counts) %% 4) != 0) {
      stop(paste0("Length of response counts should be divisible by 4, but is: ", length(counts)))
    }

    L <- length(counts) / 2

    c(
      counts[1:L] / sum(counts[1:L]),
      counts[(L + 1):(2 * L)] / sum(counts[(L + 1):(2 * L)])
    )
  } else if (is.matrix(counts)) {
    if ((ncol(counts) %% 4) != 0) {
      stop(paste0("Length of response counts should be divisible by 4, but is: ", ncol(counts)))
    }

    L <- ncol(counts) / 2
    cbind(
      counts[, 1:L, drop = FALSE] /
        rowSums(counts[, 1:L, drop = FALSE]),
      counts[, (L + 1):(2 * L), drop = FALSE] /
        rowSums(counts[, (L + 1):(2 * L), drop = FALSE])
    )
  } else {
    stop(paste0("`counts` is of type '", class(counts), "', expected vector or matrix."))
  }
}

#' Aggregate `data` by `response`, `confidence`, and other columns
#'
#' Counts number of rows in `data` with unique combinations values in the
#' columns `response`, `confidence`, and any other columns in `...`.
#'
#' @param data The dataframe to aggregate
#' @param ... Grouping columns in `data`.
#' These columns will be converted to factors.
#' @param .response The name of the resulting column containing trial counts
#' @param K The number of confidence levels in `data`.
#' If `NULL`, this is estimated from `data`.
#' @returns A tibble with one row per combination of the variables in `...`,
#' and another column named by the value of `.response` containing trial counts.
#' For \eqn{K} confidence levels, this will be an \eqn{N \times K*4} matrix, such that the
#' columns represent (for stimulus \eqn{S}, type 1 response \eqn{R}, and
#' type 2 response \eqn{C}):
#' \deqn{
#'  [N_{S=0, R=0, C=K}, \ldots, N_{S=0, R=0, C=1}, \\
#'  N_{S=0, R=1, C=1}, \ldots, N_{S=0, R=1, C=K}, \\
#'  N_{S=1, R=0, C=K}, \ldots, N_{S=1, R=0, C=1}, \\
#'  N_{S=1, R=1, C=1}, \ldots, N_{S=1, R=1, C=K}] \\
#' }{
#' [N(R=0, C=K, S=0), ..., N(R=0, C=1, S=0),
#'  N(R=1, C=1, S=0), ..., N(R=1, C=K, S=0),
#'  N(R=0, C=K, S=1), ..., N(R=0, C=1, S=1),
#'  N(R=1, C=1, S=1), ..., N(R=1, C=K, S=1)]
#' }
#' @examples
#' # aggregate a dataset without grouping factors
#' d <- sim_metad()
#' aggregate_metad(d)
#'
#' # aggregate a dataset with grouping factors
#' d2 <- sim_metad_condition()
#' aggregate_metad(d2, condition)
#'
#' # can also aggregate ignoring grouping factors
#' aggregate_metad(d2)
#' @export
aggregate_metad <- function(data, ..., .response = "N", K = NULL) {
  if (nrow(data) == 0) {
    ## generate zeros if empty
    if (is.null(K)) {
      stop("When using an empty dataset, `K` must not be null.")
    } else if (K <= 1) {
      stop("Number of confidence levels (`K`) must be greater than 1.")
    }

    data <- tidyr::expand_grid(..., stimulus = 0:1, response = 0:1, confidence = 1:K) |>
      mutate(
        joint_response = factor(joint_response(.data$response, .data$confidence, K)),
        n = 0
      ) |>
      select(-"response", -"confidence") |>
      arrange(..., .data$stimulus, .data$joint_response)
  } else {
    ## aggregate data if non-empty

    # number of confidence levels
    if (is.null(K)) {
      K <- n_distinct(data$confidence)
    }

    if (K <= 1) {
      stop("Number of confidence levels (`K`) must be greater than 1.")
    }

    data <- data |>
      ungroup() |>
      mutate(
        response = as.integer(as.character(.data$response)),
        confidence = as.integer(as.character(.data$confidence)),
        joint_response = factor(
          joint_response(.data$response, .data$confidence, K),
          levels = 1:(2 * K)
        ),
        stimulus = factor(.data$stimulus),
        across(c(...), factor)
      ) |>
      group_by(...) |>
      count(.data$stimulus, .data$joint_response, .drop = FALSE)
  }

  data <- data |>
    tidyr::pivot_wider(
      names_from = c("stimulus", "joint_response"),
      values_from = "n",
      names_prefix = glue::glue("{.response}_")
    ) |>
    mutate(
      "{.response}_0" := sum(c_across(starts_with(glue::glue("{.response}_0_"),
        ignore.case = FALSE
      ))),
      "{.response}_1" := sum(c_across(starts_with(glue::glue("{.response}_1_"),
        ignore.case = FALSE
      )))
    )

  # convert counts into a matrix-column
  tibble(
    select(
      data, ...,
      all_of(c(
        glue::glue("{.response}_0"),
        glue::glue("{.response}_1")
      ))
    ),
    "{.response}" := data |> ungroup() |>
      select(matches(glue::glue("{.response}_([[:digit:]]+)_([[:digit:]]+)"),
        ignore.case = FALSE
      )) |>
      as.matrix()
  )
}

#' Fit the meta-d' model using `brms` package
#'
#' This function is a wrapper around [brms::brm()] using a custom family for the
#' meta-d' model.
#'
#' @param formula A model formula for some or all parameters of the `metad` brms family.
#' To display all parameter names for a model with `K` confidence levels, use `metad(K)`.
#' @param data A tibble containing the data to fit the model.
#' * If `aggregate`==TRUE, `data` should have one row per observation with
#' columns `stimulus`, `response`, `confidence`, and any other variables in `formula`
#' * If `aggregate`==FALSE, it should be aggregated to have one row per cell of
#' the design matrix, with joint type 1/type 2 response counts in a matrix column
#' (see [aggregate_metad()]).
#' @param ... Additional parameters passed to the `brm` function.
#' @param aggregate If `TRUE`, automatically aggregate `data` by the variables
#' included in `formula` using [aggregate_metad()].
#' Otherwise, `data` should already be aggregated.
#' @param K The number of confidence levels. By default, this is estimated from the data.
#' @param distribution The noise distribution to use for the signal detection model.
#' By default, uses a normal distribution with a mean parameterized by `dprime`.
#' @param metac_absolute If `TRUE`, fix the type 2 criterion to be equal to the type 1 criterion.
#' Otherwise, equate the criteria relatively such that metac/metadprime = c/dprime.
#' @param stanvars Additional `stanvars` to pass to the model code, for example to define an alternative
#' distribution or a custom model prior (see [brms::stanvar()]).
#' @examples
#' # fit a basic model on simulated data
#' # running few iterations so example runs quickly, use more in practice
#' fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#'
#' @export
fit_metad <- function(formula, data, ..., aggregate = TRUE, K = NULL,
                      distribution = "normal", metac_absolute = TRUE, stanvars = NULL) {
  data.aggregated <- NULL

  # ensure formula is a brmsformula
  if (!("brmsformula" %in% attr(formula, "class"))) {
    formula <- brms::bf(formula)
  }

  # determine response variable
  .response <- all.vars(formula$formula)[attr(terms(formula$formula), "response")]

  # aggregate data by formula terms
  if (aggregate) {
    if (is.null(K)) {
      K <- n_distinct(data$confidence)
    }
    if (K <= 1) {
      stop("Number of confidence levels (`K`) must be greater than 1.")
    }

    # get a list of variables by which to aggregate
    terms <- all.vars(brms::brmsterms(brms::bf(formula, family = metad(K)))$allvars)
    terms <- syms(terms[!(terms %in% c(.response, "Intercept"))])
    data.aggregated <- aggregate_metad(data, !!!terms, .response = .response)
  } else {
    if (is.null(K)) {
      K <- ncol(pull(data, .response)) / 4
    }
    if (K <= 1) {
      stop("Number of confidence levels (`K`) must be greater than 1.")
    }
    data.aggregated <- data
  }

  if (K < 2) {
    stop(glue::glue("Error: must have at least one confidence level (found {{K}})."))
  }

  # add metad stanvars to any user-defined stanvars
  sv <- brms::stanvar(
    scode = stancode_metad(K,
      distribution = distribution,
      metac_absolute = metac_absolute
    ),
    block = "functions"
  )
  if (!is.null(stanvars)) {
    sv <- sv + stanvars
  }

  brms::brm(formula, data.aggregated,
    family = metad(K, distribution = distribution, metac_absolute = metac_absolute),
    stanvars = sv, ...
  )
}

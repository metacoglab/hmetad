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
#' @param data The data frame to aggregate
#' @param ... Grouping columns in `data`.
#' These columns will be converted to factors.
#' @param .stimulus The name of "stimulus" column
#' @param .response The name of "response" column
#' @param .confidence The name of "confidence" column
#' @param .joint_response The name of "joint_response" column
#' @param .name The name of the resulting column containing trial counts
#' @param K The number of confidence levels in `data`.
#' If `NULL`, this is estimated from `data`.
#'
#' @details
#' The data frame `data` must have one column with the name given by `.stimulus`.
#' Additionally, it must have either:
#'   * Two columns with names given by `.response` and `.confidence`
#'   * One column with the name given by `.joint_response`
#'
#' Finally, it must also have columns for any additional variables in `...`.
#'
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
#'
#' # aggregate data with only `joint_response` column
#' library(dplyr)
#' d |>
#'   ungroup() |>
#'   mutate(joint_response = joint_response(
#'     response, confidence,
#'     n_distinct(confidence)
#'   )) |>
#'   select(-response, -confidence) |>
#'   aggregate_metad()
#'
#' @export
aggregate_metad <- function(
  data, ..., .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", .joint_response = "joint_response",
  .name = "N", K = NULL
) {
  if (nrow(data) == 0) {
    ## generate zeros if empty
    if (is.null(K)) {
      stop("When using an empty dataset, `K` must not be null.")
    } else if (K <= 1) {
      stop("Number of confidence levels (`K`) must be greater than 1.")
    }

    data <- tidyr::expand_grid(..., "{.stimulus}" := 0:1, "{.response}" := 0:1, "{.confidence}" := 1:K) |>
      mutate(
        "{.joint_response}" := factor(joint_response(!!sym(.response), !!sym(.confidence), K)),
        n = 0
      ) |>
      select(-!!sym(.response), -!!sym(.confidence)) |>
      arrange(..., !!sym(.stimulus), !!sym(.joint_response))
  } else {
    ## aggregate data if non-empty

    ## fill in missing columns
    if (!(.joint_response %in% names(data))) {
      if (!(.response %in% names(data) && .confidence %in% names(data))) {
        stop(paste0(
          'Data must have a column called "', .joint_response,
          '", or two columns called "', .response, '" and "', .confidence, '".'
        ))
      }

      # number of confidence levels
      if (is.null(K)) {
        K <- data |>
          pull(!!sym(.confidence)) |>
          n_distinct()
      }

      ## add joint_response column
      data <- data |>
        mutate("{.joint_response}" := factor(
          joint_response(!!sym(.response), !!sym(.confidence), K),
          levels = 1:(2 * K)
        ))
    } else {
      # number of confidence levels
      if (is.null(K)) {
        K <- as.integer((data |> pull(!!sym(.joint_response)) |> n_distinct()) / 2)
      }

      if (!(.response %in% names(data))) {
        ## add response column
        data <- data |>
          mutate("{.response}" := factor(type1_response(!!sym(.joint_response), K)))
      }

      if (!(.confidence %in% names(data))) {
        ## add confidence column
        data <- data |>
          mutate("{.confidence}" := factor(type2_response(!!sym(.joint_response), K)))
      }
    }

    if (K <= 1) {
      stop("Number of confidence levels (`K`) must be greater than 1.")
    }

    if (!(.stimulus %in% names(data))) {
      stop(paste0('Data must have a column called "', .stimulus, '".'))
    }

    if (!all(names(enquos(..., .named = TRUE)) %in% names(data))) {
      stop(paste0(
        "Data must have the following columns: ",
        stringr::str_flatten_comma(sapply(
          names(enquos(..., .named = TRUE)),
          function(s) paste0('"', s, '"')
        )),
        "."
      ))
    }

    data <- data |>
      ungroup() |>
      mutate(
        "{.stimulus}" := factor(!!sym(.stimulus)),
        across(c(...), factor)
      ) |>
      group_by(...) |>
      count(!!sym(.stimulus), !!sym(.joint_response), .drop = FALSE)
  }

  data <- data |>
    tidyr::pivot_wider(
      names_from = all_of(c(.stimulus, .joint_response)),
      values_from = "n",
      names_prefix = glue::glue("{.name}_")
    ) |>
    mutate(
      "{.name}_0" := sum(c_across(starts_with(glue::glue("{.name}_0_"),
        ignore.case = FALSE
      ))),
      "{.name}_1" := sum(c_across(starts_with(glue::glue("{.name}_1_"),
        ignore.case = FALSE
      )))
    )

  # convert counts into a matrix-column
  tibble(
    select(
      data, ...,
      all_of(c(
        glue::glue("{.name}_0"),
        glue::glue("{.name}_1")
      ))
    ),
    "{.name}" := data |> ungroup() |>
      select(matches(glue::glue("{.name}_([[:digit:]]+)_([[:digit:]]+)"),
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
#' @param formula A model formula for some or all parameters of the `metad` brms
#'   family. To display all parameter names for a model with `K` confidence
#'   levels, use `metad(K)`.
#' @param data A tibble containing the data to fit the model.
#' * If `aggregate`==TRUE, `data` should have one row per observation with
#'   columns `stimulus`, `response`, `confidence`, and any other variables in
#'   `formula`
#' * If `aggregate`==FALSE, it should be aggregated to have one row per cell of
#'   the design matrix, with joint type 1/type 2 response counts in a matrix
#'   column (see [aggregate_metad()]).
#' @param ... Additional parameters passed to the `brm` function.
#' @param aggregate If `TRUE`, automatically aggregate `data` by the variables
#'   included in `formula` using [aggregate_metad()]. Otherwise, `data` should
#'   already be aggregated.
#' @param .stimulus The name of "stimulus" column
#' @param .response The name of "response" column
#' @param .confidence The name of "confidence" column
#' @param .joint_response The name of "joint_response" column
#' @param K The number of confidence levels. By default, this is estimated from
#'   the data.
#' @param distribution The noise distribution to use for the signal detection
#'   model. By default, uses a normal distribution with a mean parameterized by
#'   `dprime`.
#' @param metac_absolute If `TRUE`, fix the type 2 criterion to be equal to the
#'   type 1 criterion. Otherwise, equate the criteria relatively such that
#'   metac/metadprime = c/dprime.
#' @param stanvars Additional `stanvars` to pass to the model code, for example
#'   to define an alternative distribution or a custom model prior (see
#'   [brms::stanvar()]).
#' @param categorical If `FALSE` (default), use the multinomial likelihood over
#'   aggregated data. If `TRUE`, use the categorical likelihood over individual
#'   trials.
#' @examples
#' # fit a basic model on simulated data
#' # running few iterations so example runs quickly, use more in practice
#' fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#'
#' @export
fit_metad <- function(formula, data, ..., aggregate = TRUE,
                      .stimulus = "stimulus", .response = "response",
                      .confidence = "confidence", .joint_response = "joint_response",
                      K = NULL,
                      distribution = "normal", metac_absolute = TRUE, stanvars = NULL,
                      categorical = FALSE) {
  data.aggregated <- data

  # ensure formula is a brmsformula
  if ("formula" %in% class(formula)) {
    formula <- bf(formula)
  } else if ("mvbrmsformula" %in% class(formula)) {
    stop("Error: multivariate brms formulas not yet supported.")
  }

  # determine response variable
  .name <- all.vars(formula$formula)[attr(terms(formula$formula), "response")]

  # determine number of confidence levels
  if (is.null(K)) {
    if (aggregate && !categorical) {
      K <- n_distinct(data$confidence)
    } else if (!categorical) {
      # if data is already aggregated, count number of columns
      K <- ncol(pull(data, .name)) / 4
    } else {
      # for categorical models, use joint_response column
      K <- n_distinct(pull(data, .name)) / 2
    }
  }

  if (K <= 1) {
    stop("Number of confidence levels (`K`) must be greater than 1.")
  }

  ## convert the formula to use the metad family
  formula <- bf(formula,
    family = metad(K,
      distribution = distribution,
      metac_absolute = metac_absolute,
      categorical = categorical
    )
  )

  # aggregate data by formula terms
  if (aggregate && !categorical) {
    # get a list of variables by which to aggregate
    terms <- all.vars(brmsterms(formula)$allvars)
    terms <- syms(terms[!(terms %in% c(.name, "Intercept"))])
    data.aggregated <- aggregate_metad(
      data, !!!terms,
      .stimulus = .stimulus, .response = .response,
      .confidence = .confidence, .joint_response = .joint_response,
      .name = .name,
    )
  }

  # add metad stanvars to any user-defined stanvars
  sv <- stanvars_metad(K,
    distribution = distribution,
    metac_absolute = metac_absolute,
    categorical = categorical
  )
  if (!is.null(stanvars)) {
    sv <- sv + stanvars
  }

  brm(formula, data.aggregated, stanvars = sv, ...)
}

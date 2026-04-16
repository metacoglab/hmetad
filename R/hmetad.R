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
#' try(to_signed(factor(1)))
#' tryCatch(to_unsigned(factor(1)), warning = function(w) w)
#' @rdname signed
#' @export
to_signed <- function(x) ifelse(x, 1, -1)

#' @rdname signed
#' @export
to_unsigned <- function(x) as.numeric(x > 0)

#' Convert between separate and joint type 1/type 2 responses
#'
#' @description Confidence ratings and decisions are collected in one of two
#' ways.
#'  * For separate ratings, there will be a type 1 response (\eqn{R \in \{0, 1\}}) and a
#' type 2 response (\eqn{C \in [1, K]}).
#'  * For joint ratings, there is instead a combined type 1/type 2 response
#' (\eqn{J \in [1, 2K]}), with values in \eqn{[1, K]} indicating a type 1
#' response of \eqn{0} and values in \eqn{[K+1, 2K]} indicating a type 1
#' response of \eqn{1}, with confident responses at the ends of the scale.
#'
#' `joint_response` converts separate type 1 and type 2 responses into the joint
#' format
#'
#' `type1_response` and `type2_response` convert the joint response into
#' separate responses.
#'
#' @param joint_response A joint type 1/type 2 response
#' @param response A type 1 response (`0` or `1`)
#' @param confidence A type 2 response/confidence rating (in `1:K`)
#' @param K The number of confidence levels
#' @returns A joint response (for `joint_response`), type 1 response (for
#'   `type1_response`), or type 2 response (for `type2_response`)
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
  confidence <- as.integer(confidence)
  as.integer(ifelse(response, confidence + K, K + 1 - confidence))
}

#' @rdname responses
#' @export
type1_response <- function(joint_response, K) {
  as.integer(as.integer(joint_response) > K)
}

#' @rdname responses
#' @export
type2_response <- function(joint_response, K) {
  joint_response <- as.integer(joint_response)
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

#' Infer the number of confidence levels in aggregated or non-aggregated data
#'
#' @param data A tibble containing the data to fit the model.
#' * If `aggregate`==TRUE, `data` should have one row per observation with
#'   columns `stimulus`, `response`, `confidence`, and any other variables in
#'   `formula`
#' * If `aggregate`==FALSE, it should be aggregated to have one row per cell of
#'   the design matrix, with joint type 1/type 2 response counts in a matrix
#'   column (see [aggregate_metad()]).
#' @param aggregate If `TRUE`, automatically aggregate `data` by the variables
#'   included in `formula` using [aggregate_metad()]. Otherwise, `data` should
#'   already be aggregated.
#' @param .stimulus The name of "stimulus" column
#' @param .response The name of "response" column
#' @param .confidence The name of "confidence" column
#' @param .joint_response The name of "joint_response" column
#' @param categorical If `FALSE` (default), use the multinomial likelihood over
#'   aggregated data. If `TRUE`, use the categorical likelihood over individual
#'   trials.
#' @returns An integer describing the number of confidence levels in `data`
#' @keywords internal
#' @noRd
infer_confidence_levels <- function(
  data, aggregate = TRUE, .stimulus = "stimulus", .response = "response",
  .confidence = "confidence", .joint_response = "joint_response",
  .name = "N", categorical = FALSE
) {
  # check validity of stimulus column
  if (aggregate || categorical) {
    if (!(.stimulus %in% names(data))) {
      stop(paste0('Data must have a column called "', .stimulus, '".'))
    }
    if (!all(as.integer(as.character(unique(pull(data, .stimulus)))) %in% c(0, 1))) {
      stop(
        'Stimulus column "', .stimulus, '" should only have values 0 or 1, but has values: ',
        setdiff(as.integer(as.character(unique(pull(data, .stimulus)))), c(0, 1))
      )
    }
  }

  K <- NULL
  if (categorical) {
    # for categorical models, use joint_response column
    K <- max(as.integer(pull(data, .name)))

    if (K %% 2 != 0) {
      stop(paste0(
        "Joint response column `", .name,
        "` must have an even number of levels, but has ", K, " levels."
      ))
    }

    K <- as.integer(K / 2)
  } else if (!aggregate) {
    if (!(.name %in% names(data))) {
      stop(paste0("Aggregated data must have a column called `", .name, "`."))
    }

    # if data is already aggregated, count number of columns
    K <- ncol(pull(data, .name))
    if (K %% 4 != 0) {
      stop(paste0(
        "Joint response count matrix `", .name,
        "` in aggregated data must have a number of columns divisible by 4, but has ",
        K, " columns."
      ))
    }
    K <- as.integer(K / 4)
  } else if (.confidence %in% names(data) && .response %in% names(data)) {
    K <- max(as.integer(pull(data, .confidence)))

    ## validate .confidence and .response columns
    if (!all(as.integer(as.character(unique(pull(data, .response)))) %in% c(0, 1))) {
      stop(
        "Response column `", .response, "` should only have values 0 or 1, but has values: ",
        as.integer(as.character(unique(pull(data, .response))))
      )
    } else if (!all(as.integer(unique(pull(data, .confidence))) %in% seq_len(K))) {
      stop(
        "Confidence column `", .confidence, "` should only have values 1:", K, ", but has values: ",
        as.integer(as.character(unique(pull(data, .confidence))))
      )
    }
  } else if (.joint_response %in% names(data)) {
    K <- max(as.integer(pull(data, .joint_response)))

    # validate joint response column
    if ((K %% 2) != 0) {
      stop(paste0(
        "Joint response column `", .joint_response,
        "` must have an even number of levels, but has ", K, " levels"
      ))
    } else if (!all(as.integer(unique(pull(data, .joint_response))) %in% seq_len(K))) {
      stop(
        "Joint response column `", .joint_response,
        "` should only have values 1:", K, ", but has values: ",
        as.integer(as.character(unique(pull(data, .joint_response))))
      )
    }

    K <- as.integer(K / 2)
  } else {
    stop(paste0(
      "Data must have a column called `", .joint_response,
      "`, or two columns called `", .response, "` and `", .confidence, "`."
    ))
  }

  # check validity of K
  if (K <= 1) {
    stop("Number of confidence levels (`K`) must be greater than 1.")
  }

  message(paste0("`hmetad` has inferred that there are K=", K, " confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`"))

  K
}

#' Aggregate `data` by `response`, `confidence`, and other columns
#'
#' Counts number of rows in `data` with unique combinations values in the
#' columns `response`, `confidence`, and any other columns in `...`.
#'
#' @param data The data frame to aggregate
#' @param ... Grouping columns in `data`. These columns will be converted to
#'   factors.
#' @param .stimulus The name of "stimulus" column
#' @param .response The name of "response" column
#' @param .confidence The name of "confidence" column
#' @param .joint_response The name of "joint_response" column
#' @param .name The name of the resulting column containing trial counts
#' @param K The number of confidence levels in `data`. If `NULL`, this is
#'   estimated from `data` using the maximum value of either the confidence
#'   column or joint response column.
#' @details The data frame `data` must have one column with the name given by
#' `.stimulus`. Additionally, it must have either:
#'   * Two columns with names given by `.response` and `.confidence`
#'   * One column with the name given by `.joint_response`
#'
#' Finally, it must also have columns for any additional variables in `...`.
#'
#' @returns A tibble with one row per combination of the variables in `...`, and
#'   another column named by the value of `.response` containing trial counts.
#'   For \eqn{K} confidence levels, this will be an \eqn{N \times K*4} matrix,
#'   such that the columns represent (for stimulus \eqn{S}, type 1 response
#'   \eqn{R}, and type 2 response \eqn{C}):
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
#'     max(as.integer(confidence))
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
        "{.joint_response}" := joint_response(!!sym(.response), !!sym(.confidence), K),
        n = 0
      ) |>
      select(-!!sym(.response), -!!sym(.confidence)) |>
      arrange(..., !!sym(.stimulus), !!sym(.joint_response))
  } else {
    # aggregate data if non-empty

    # infer number of confidence levels
    if (is.null(K)) {
      K <- infer_confidence_levels(
        data,
        aggregate = TRUE, categorical = FALSE,
        .stimulus = .stimulus, .response = .response, .confidence = .confidence,
        .joint_response = .joint_response, .name = .name
      )
    }

    # create joint response column if it doesn't exist
    if (!(.joint_response %in% names(data))) {
      if (!(.confidence %in% names(data) && .response %in% names(data))) {
        stop(paste0(
          "Data must have a column called `", .joint_response,
          "`, or two columns called `", .response, "` and `", .confidence, "`."
        ))
      }

      data <- data |>
        mutate("{.joint_response}" := joint_response(!!sym(.response), !!sym(.confidence), K))
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

    # aggregate data, filling in empty cells with zero
    data <- data |>
      ungroup() |>
      count(!!sym(.stimulus), !!sym(.joint_response), ...) |>
      tidyr::complete(
        data |>
          ungroup() |>
          tidyr::expand(
            "{.stimulus}" := c(0L, 1L),
            "{.joint_response}" := seq_len(2 * K),
            ...
          ),
        fill = list(n = 0)
      )
  }

  # convert data to wide format (one row per cell in ...)
  data <- data |>
    tidyr::pivot_wider(
      names_from = all_of(c(.stimulus, .joint_response)),
      values_from = "n",
      names_prefix = glue::glue("{.name}_")
    ) |>
    rowwise() |>
    mutate(
      "{.name}_0" := sum(c_across(starts_with(glue::glue("{.name}_0_"),
        ignore.case = FALSE
      ))),
      "{.name}_1" := sum(c_across(starts_with(glue::glue("{.name}_1_"),
        ignore.case = FALSE
      )))
    ) |>
    ungroup()

  # if data is not empty, remove any cells with no observations
  if (!all(data[, glue::glue("{.name}_0")] == 0) ||
    !all(data[, glue::glue("{.name}_1")] == 0)) {
    data <- data |>
      filter(
        !!sym(glue::glue("{.name}_0")) > 0 |
          !!sym(glue::glue("{.name}_1")) > 0
      )
  }

  # convert counts into a matrix-column
  tibble(
    select(
      data, ...,
      all_of(c(
        glue::glue("{.name}_0"),
        glue::glue("{.name}_1")
      ))
    ),
    "{.name}" := data |>
      ungroup() |>
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
#' @param K The number of confidence levels in `data`. If `NULL`, this is
#'   estimated from `data` using the maximum level of either the confidence
#'   column or joint response column.
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
#' @param logit If `TRUE` (default), use the logit parameterization of the
#'   likelihood over the log joint response probabilities. If `FALSE`, use the
#'   standard parameterization of the likelihood over the actual joint response
#'   probabilities. In most cases, the logit parameterization should provide
#'   more stable numerical computations, but the standard parameterization might
#'   be preferable in some settings.
#' @returns A `brmsfit` object containing the fitted model
#' @details `fit_metad(formula, data, ...)` is approximately the same as
#'   `brm(formula, data=aggregate_metad(data, ...), family=metad(...),
#'   stanvars=stanvars_metad(...), ...)`. For some models, it may often be
#'   easier to use the more explicit version than using `fit_metad`.
#' @examples
#' # check which parameters the model has
#' metad(3)
#'
#' # fit a basic model on simulated data
#' # (use `empty=true` to bypass fitting, *do not use in real analysis*)
#' fit_metad(N ~ 1, sim_metad(), empty = TRUE)
#' \donttest{
#' # fit a basic model on simulated data
#' fit_metad(N ~ 1, sim_metad())
#'
#' # fit a model with condition-level effects
#' fit_metad(
#'   bf(
#'     N ~ condition,
#'     dprime + c + metac2zero1diff + metac2zero2diff +
#'       metac2one1diff + metac2one1diff ~ condition
#'   ),
#'   data = sim_metad_condition()
#' )
#' }
#' @export
fit_metad <- function(formula, data, ..., aggregate = TRUE,
                      .stimulus = "stimulus", .response = "response",
                      .confidence = "confidence", .joint_response = "joint_response",
                      K = NULL,
                      distribution = "normal", metac_absolute = TRUE, stanvars = NULL,
                      categorical = FALSE, logit = TRUE) {
  data.aggregated <- data

  # ensure formula is a brmsformula
  if ("formula" %in% class(formula)) {
    formula <- bf(formula)
  } else if ("mvbrmsformula" %in% class(formula)) {
    stop("Multivariate brms formulas not yet supported.")
  }

  # determine response variable
  .name <- all.vars(formula$formula)[attr(terms(formula$formula), "response")]
  if (categorical) {
    # use vint variable as stimulus column for categorical models
    .stimulus <- stringr::str_extract(
      deparse1(formula$formula), 
      "^.* \\| vint\\((.*)\\) ~ .*$", 
      group=1
    )
    
    # ensure that the stimulus column exists in the model formula
    if (is.na(.stimulus)) {
      stop("Categorical models require a `vint` term in the response of the model formula. See `help('brmsformula')` for more information")
    }
  }
  
  # determine number of confidence levels
  # (run whether K is specified or not to validate data columns)
  K_inferred <- infer_confidence_levels(
    data,
    aggregate = aggregate, categorical = categorical,
    .stimulus = .stimulus, .response = .response, .confidence = .confidence,
    .joint_response = .joint_response, .name = .name
  )
  if (is.null(K)) {
   K <- K_inferred
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
      .name = .name, K = K
    )
  }

  # add metad stanvars to any user-defined stanvars
  sv <- stanvars_metad(K,
    distribution = distribution,
    metac_absolute = metac_absolute,
    categorical = categorical,
    logit = logit
  )
  if (!is.null(stanvars)) {
    sv <- sv + stanvars
  }

  brm(formula, data.aggregated, stanvars = sv, ...)
}

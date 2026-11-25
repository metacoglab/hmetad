#' Generate a covariance matrix.
#'
#' @param S A vector of standard deviations
#' @param OMEGA A correlation matrix
#' @returns an \eqn{N \times N} covariance matrix, where `N = length(S)`.
#' @examples
#' sds <- c(1, 2)
#' corrs <- matrix(c(1, .5, .5, 1), nrow = 2)
#' cov_matrix(sds, corrs)
#' @export
cov_matrix <- function(S, OMEGA) {
  if (!is.vector(S) || !is.numeric(S) || any(S <= 0)) {
    stop("`S` must be a positive vector.")
  }
  if (!is.matrix(OMEGA) || any(OMEGA < -1) || any(OMEGA > 1) ||
    any(diag(OMEGA) != 1) || nrow(OMEGA) != ncol(OMEGA) ||
    any(OMEGA != t(OMEGA))
  ) {
    stop("`OMEGA` must by a symmetric correlation matrix.")
  }
  if (length(S) != nrow(OMEGA)) {
    stop("Dimensions of `S` and `OMEGA` must match.")
  }
  
  if (length(S) == 1) {
    S %*% OMEGA %*% S
  } else {
    diag(S) %*% OMEGA %*% diag(S)
  }
}

#' Generate a correlation matrix with all off-diagonal values equal to `r`
#'
#' @param r The correlation to fill in the matrix off-diagonals
#' @param nrow The number of rows (and columns) of the resulting matrix
#' @returns An `[nrow x nrow]` matrix with values along the diagonal equal
#' to `1` and values off of the diagonal equal to `r`
#' @examples
#' cor_matrix(0, nrow = 3)
#'
#' cor_matrix(-.5, nrow = 4)
#' @export
cor_matrix <- function(r, nrow = 2) {
  if (!is.numeric(r) || length(r) != 1 || !between(r, -1, 1)) {
    stop("`r` must be a single correlation between -1 and 1.")
  }
  if (nrow <= 1) {
    stop("`nrow` must be greater than 1.")
  }
  diag(1 - r, nrow) + r
}

#' Sample from a matrix-normal distribution
#' @param mu a matrix of means
#' @param L_sigma_rows the Cholesky-decomposed covariance matrix for the rows
#' @param L_sigma_cols the Cholesky-decomposed covariance matrix for the columns
#' @returns A single sample from a matrix-normal distribution with mean
#' `mu` (a matrix), row-wise covariances `sigma_rows`, and column-wise
#' covariances `sigma_cols`, where `L_sigma_rows` and `L_sigma_cols` are the
#' Cholesky-decomposed covariance matrices
#' @examples
#' mu <- matrix(rep(0, 8), nrow = 4)
#' sd_rows <- rep(1, 4)
#' sd_cols <- rep(1, 2)
#' r_rows <- cor_matrix(.25, 4)
#' r_cols <- cor_matrix(.75, 2)
#' L_sigma_rows <- chol(cov_matrix(sd_rows, r_rows))
#' L_sigma_cols <- chol(cov_matrix(sd_cols, r_cols))
#' rmatrixnorm(mu, L_sigma_rows, L_sigma_cols)
#' @export
rmatrixnorm <- function(mu, L_sigma_rows, L_sigma_cols) {
  if (nrow(mu) != nrow(L_sigma_rows) || nrow(mu) != ncol(L_sigma_rows)) {
    stop("`L_sigma_rows` must be a square matrix of dimension `nrow(mu)`.")
  }
  if (ncol(mu) != ncol(L_sigma_cols) || ncol(mu) != nrow(L_sigma_cols)) {
    stop("`L_sigma_cols` must be a square matrix of dimension `ncol(mu)`.")
  }

  D <- nrow(L_sigma_rows) * nrow(L_sigma_cols)
  mu +
    L_sigma_rows %*%
    matrix(rmulti_normal(1, mu = rep(0, D), Sigma = diag(D)),
      nrow = nrow(L_sigma_rows)
    ) %*%
    L_sigma_cols
}

#' Simulate from the meta-d' model
#'
#' @description Generate a simulated dataset from the meta-d' model with
#'   sensitivity `dprime`, response bias `c`, metacognitive efficiency `M`, and
#'   distances between confidence thresholds `c2_0_diff` and `c2_1_diff` (for
#'   the two responses).
#' @param N_trials Total number of trials to simulate. Half of these trials will
#'   have `stimulus=0` and half will have `stimulus=1`.
#' @param dprime The sensitivity of the signal detection agent to simulate
#' @param c The response bias of the signal detection agent to simulate
#' @param M The metacognitive efficiency of the agent, where negative values
#'   indicate below chance metacognitive sensitivity, `0` indicates an absence
#'   of metacognitive sensitivity, values between `0` and `1` indicate
#'   metacognitive inefficiency, 1` indicates optimal metacognitive sensitivity,
#'   and values greater than `1` indicate metacognitive hyper-efficiency.
#'
#'   If `response_specific=FALSE`, this argument is ignored in favor of
#'   `M_0` and `M_1` for `"0"` and `"1"` responses, respectively.
#' @param c2_0_diff,c2_1_diff Distances between confidence thresholds for `"0"`
#'   and `"1"` responses, such that `meta_c2_0 = meta_c - cumsum(c2_0_diff)` and
#'   `meta_c2_1 = meta_c + cumsum(c2_1_diff)`.
#' @param metac_absolute Determines how to fix the type 1 threshold for modeling
#'   confidence ratings. If `metac_absolute=TRUE`, `meta_c = c`. Otherwise,
#'   `meta_c = M * c`.
#' @param summarize Aggregate the data?
#' * If `summarize=FALSE`, returns a dataset with one row per observation.
#' * If `summarize=TRUE`, returns an aggregated
#'   dataset where `n` is the number of observations per response, accuracy, and
#'   confidence level.
#' @param lcdf,lccdf The log (complement) cumulative distribution function of
#'   the underlying signal distribution. By default, uses a `normal(+/-dprime/2,
#'   1)` distribution.
#' @param M_0,M_1 response-specific values of metacognitive efficiency.
#'   Ignored if `response_specific=FALSE` (default).
#' @param response_specific if `FALSE` (default), uses the same metacognitive
#'   efficiency (`M`) for both Type I responses. If `TRUE`, uses `M_0` and `M_1`
#'   as response-specific metacognitive efficiencies for
#'   `"0"` and `"1"` responses, respectively.
#' @returns A simulated dataset of type 1 responses and confidence ratings, with
#'   columns:
#'   * `trial`: the simulated trial number
#'   * `stimulus`: the value of the stimulus on each trial (either `0` or `1`)
#'   * `response`: the simulated type 1 response (either `0` or `1`)
#'   * `correct`: whether `stimulus==response` (either `0` or `1`)
#'   * `confidence`: the simulated type 2 response (from `1` to `length(c2_0_diff)+1`)
#'   * `dprime:theta_2`: the simulated agent's parameter values
#'
#'   If `summarize=TRUE`, the `trial` column is replaced with an `n` column
#'   indicating the number of simulated type 1/type 2 responses for each
#'   possible value.
#' @examples
#' sim_metad(N_trials = 10)
#' sim_metad(N_trials = 10000, summarize = TRUE)
#' sim_metad(N_trials = 10, c2_0_diff = 1, c2_1_diff = 1)
#' @export
sim_metad <- function(
  N_trials = 100, dprime = 1, c = 0,
  M = 1, c2_0_diff = rep(.5, 3), c2_1_diff = rep(.5, 3),
  metac_absolute = TRUE, summarize = FALSE,
  lcdf = normal_lcdf, lccdf = normal_lccdf,
  M_0 = 1, M_1 = 1, response_specific = FALSE
) {
  if (N_trials <= 0) {
    stop("Error: `N_trials` must be greater than 0.")
  }
  if (!all(length(dprime) == 1, length(c) == 1, is.numeric(dprime), is.numeric(c))) {
    stop("Error: `dprime`, `c`, and `M` must be single numbers.")
  }
  if (!is.numeric(c2_0_diff) || !is.numeric(c2_1_diff) ||
    length(c2_0_diff) != length(c2_1_diff) ||
    !all(c2_0_diff > 0) || !all(c2_1_diff > 0)
  ) {
    stop("Error: `c2_0_diff` and `c2_1_diff` must be positive vectors of the same length.")
  }
  
  if (response_specific) {
    if (length(M_0) != 1 || length(M_1) != 1 || !is.numeric(M_0) || !is.numeric(M_1)) {
      stop("Error: `M_0` and `M_1` must single numbers.")
    }
  } else {
    if (length(M) != 1 || !is.numeric(M)) {
      stop("Error: `M` must be a single number.")
    }
    M_0 <- M
    M_1 <- M
  }
  
  meta_dprime_0 <- M_0 * dprime
  meta_dprime_1 <- M_1 * dprime
  meta_c <- NULL
  if (metac_absolute) {
    meta_c_0 <- c
    meta_c_1 <- c
  } else {
    meta_c_0 <- M_0 * c
    meta_c_1 <- M_1 * c
  }
  meta_c2_0 <- meta_c_0 - cumsum(c2_0_diff)
  meta_c2_1 <- meta_c_1 + cumsum(c2_1_diff)
  K <- length(meta_c2_0) + 1

  d <- tidyr::expand_grid(
    stimulus = 0:1, response = 0:1, confidence = 1:K
  ) |>
    mutate(
      correct = as.integer(.data$stimulus == .data$response),
      joint_response = joint_response(.data$response, .data$confidence, K)
    ) |>
    arrange(.data$stimulus, .data$joint_response) |>
    group_by(.data$stimulus) |>
    mutate(
      theta = rsmetad_pmf(
        first(.data$stimulus), dprime, c,
        meta_dprime_0, meta_dprime_1, meta_c_0, meta_c_1,
        meta_c2_0, meta_c2_1, lcdf = lcdf, lccdf = lccdf
      ),
      theta_1 = ifelse(
        .data$response,
        exp(lccdf(c, to_signed(.data$stimulus) * dprime / 2)),
        exp(lcdf(c, to_signed(.data$stimulus) * dprime / 2))
      ),
      theta_2 = .data$theta / .data$theta_1,
      n = as.vector(rmultinom(1, N_trials / 2, .data$theta))
    ) |>
    mutate(
      dprime = dprime, c = c,
      meta_dprime = ifelse(.data$response, meta_dprime_1, meta_dprime_0),
      M = ifelse(.data$response, M_1, M_0),
      meta_c2_0 = list(meta_c2_0),
      meta_c2_1 = list(meta_c2_1)
    ) |>
    select(
      "stimulus", "response", "correct", "confidence", "n", "dprime", "c",
      "meta_dprime", "M", "meta_c2_0", "meta_c2_1", "theta", "theta_1", "theta_2"
    ) |>
    arrange(.data$stimulus, .data$response, .data$confidence)

  if (summarize) {
    d
  } else {
    d |>
      tidyr::uncount(.data$n) |>
      mutate(trial = row_number()) |>
      select(-"n") |>
      relocate("trial") |>
      group_by(.data$stimulus, .data$response, .data$confidence)
  }
}

#' Simulate from the meta-d' model across separate conditions
#' @description Generate a simulated dataset across separate conditions from the
#'   meta-d' model with sensitivity `dprime`, response bias `c`, metacognitive
#'   efficiency `M`, and distances between confidence thresholds `c2_0_diff` and
#'   `c2_1_diff` (for the two responses).
#' @param N_trials Total number of trials to simulate. Half of these trials will
#'   have `stimulus=0` and half will have `stimulus=1`.
#' @param dprime The sensitivity of the signal detection agent to simulate
#' @param c The response bias of the signal detection agent to simulate
#' @param M The metacognitive efficiency of the agent, where negative values
#'   indicate below chance metacognitive sensitivity, `0` indicates an absence
#'   of metacognitive sensitivity, values between `0` and `1` indicate
#'   metacognitive inefficiency, 1` indicates optimal metacognitive sensitivity,
#'   and values greater than `1` indicate metacognitive hyper-efficiency.
#'
#'   If `response_specific=FALSE`, this argument is ignored in favor of
#'   `M_0` and `M_1` for `"0"` and `"1"` responses, respectively.
#' @param c2_0_diff,c2_1_diff Distances between confidence thresholds for `"0"`
#'   and `"1"` responses, such that `meta_c2_0 = meta_c - cumsum(c2_0_diff)` and
#'   `meta_c2_1 = meta_c + cumsum(c2_1_diff)`.
#' @param metac_absolute Determines how to fix the type 1 threshold for modeling
#'   confidence ratings. If `metac_absolute=TRUE`, `meta_c = c`. Otherwise,
#'   `meta_c = M * c`.
#' @param summarize Aggregate the data? If `summarize=FALSE`, returns a dataset
#'   with one row per observation. If `summarize=TRUE`, returns an aggregated
#'   dataset where `n` is the number of observations per response, accuracy, and
#'   confidence level.
#' @param lcdf,lccdf The log (complement) cumulative distribution function of
#'   the underlying signal distribution. By default, uses a `normal(+/-dprime/2,
#'   1)` distribution.
#' @param M_0,M_1 response-specific values of metacognitive efficiency.
#'   Ignored if `response_specific=FALSE` (default).
#' @param response_specific if `FALSE` (default), uses the same metacognitive
#'   efficiency (`M`) for both Type I responses. If `TRUE`, uses `M_0` and `M_1`
#'   as response-specific metacognitive efficiencies for
#'   `"0"` and `"1"` responses, respectively.
#' @returns A simulated dataset of type 1 responses and confidence ratings, with
#'   columns:
#'   * `trial`: the simulated trial number
#'   * `condition`: the simulated condition number
#'   * `stimulus`: the value of the stimulus on each trial (either `0` or `1`)
#'   * `response`: the simulated type 1 response (either `0` or `1`)
#'   * `correct`: whether `stimulus==response` (either `0` or `1`)
#'   * `confidence`: the simulated type 2 response (from `1` to `length(c2_0_diff)+1`)
#'   * `dprime:theta_2`: the simulated agent's parameter values
#'
#'   If `summarize=TRUE`, the `trial` column is replaced with an `n` column
#'   indicating the number of simulated type 1/type 2 responses for each
#'   possible value.
#' @examples
#' sim_metad_condition(N_trials = 10)
#' sim_metad_condition(N_trials = 10000, summarize = TRUE)
#' sim_metad_condition(N_trials = 10, c2_0_diff = list(1, .5), c2_1_diff = list(1, .5))
#' @export
sim_metad_condition <- function(
  N_trials = 100, dprime = rep(1, 2), c = rep(0, 2),
  M = rep(1, 2),
  c2_0_diff = list(rep(.5, 3), rep(.5, 3)),
  c2_1_diff = list(rep(.5, 3), rep(.5, 3)),
  metac_absolute = TRUE, summarize = FALSE,
  lcdf = normal_lcdf, lccdf = normal_lccdf,
  M_0 = rep(1, 2), M_1 = rep(1, 2), response_specific=FALSE
) {
  check_installed("purrr", reason = "to use `purrr::pmap`")

  # setup response-specific M-ratio
  if (!response_specific) {
    M_0 <- M
    M_1 <- M
  }
  
  tibble(
    condition = seq_along(dprime),
    dprime = dprime, c = c,
    M_0 = M_0, M_1 = M_1,
    c2_0_diff = c2_0_diff, c2_1_diff = c2_1_diff
  ) |>
    mutate(data = purrr::pmap(
      list(
        .data$dprime, .data$c, .data$M_0, .data$M_1,
        .data$c2_0_diff, .data$c2_1_diff
      ),
      function(d, c , m0, m1, c2_0, c2_1) sim_metad(
        N = N_trials, dprime = d, c = c,
        M_0 = m0, M_1 = m1,
        c2_0_diff = c2_0, c2_1_diff = c2_1,
        metac_absolute = metac_absolute, summarize = summarize,
        lcdf = lcdf, lccdf = lccdf,
        response_specific=TRUE
      )
    )) |>
    select("condition", "data") |>
    tidyr::unnest("data")
}


#' Simulate from the hierarchical meta-d' model
#'
#' @description Generate a simulated dataset across participants from the
#'   meta-d' model with sensitivity `dprime`, response bias `c`, metacognitive
#'   efficiency `M`, and distances between confidence thresholds `c2_0_diff` and
#'   `c2_1_diff` (for the two responses).
#' @param N_trials,N_participants Total number of participants and trials to
#'   simulate per participant. Half of these trials will have `stimulus=0` and
#'   half will have `stimulus=1`.
#' @param mean_dprime,sd_dprime The mean and standard deviation of sensitivities
#'   of the signal detection agents to simulate
#' @param mean_c,sd_c The mean and standard deviation of response bias of the
#'   signal detection agents to simulate
#' @param mean_M,sd_M The mean and standard deviation of metacognitive efficiency
#'   of the agents. If `allow_negative_values==TRUE` (default), M-ratio is
#'   simulated on the logarithmic scale, where `0` indicates optimal
#'   metacognitive sensitivity, negative numbers indicate metacognitive
#'   inefficiency, and positive numbers indicate metacognitive hyper-efficiency.
#'   Otherwise, M-ratio is modeled on its natural scale to allow negative
#'   values.
#' @param mean_z_c2_0,mean_z_c2_1 Mean distance between confidence thresholds for
#'   `"0"` and `"1"` responses on the log_scale, such that `meta_c2_0 = meta_c -
#'   cumulative_sum(exp(z_c2_0))` and `meta_c2_1 = meta_c +
#'   cumulative_sum(exp(z_c2_1))`.
#' @param sd_z_c2_0,sd_z_c2_1 SD of log distances between confidence thresholds
#'   for `"0"` and `"1"` responses on the log_scale.
#' @param r_z_c2_0,r_z_c2_1 Correlation of log distances between confidence
#'   thresholds for `"0"` and `"1"` responses on the log_scale.
#' @param metac_absolute Determines how to fix the type 1 threshold for modeling
#'   confidence ratings. If `metac_absolute=TRUE`, `meta_c = c`. Otherwise,
#'   `meta_c = M * c`.
#' @param allow_negative_values If `allow_negative_values=FALSE` (default),
#'   M-ratio is simulated using a normal distribution on the logarithmic scale
#'   to prohibit negative values. If `allow_negative_values=TRUE`, then M-ratio
#'   is simulated on its natural scale, allowing negative values.
#' @param summarize Aggregate the data? If `summarize=FALSE`, returns a dataset
#'   with one row per observation. If `summarize=TRUE`, returns an aggregated
#'   dataset where `n` is the number of observations per response, accuracy, and
#'   confidence level.
#' @param lcdf The log cumulative distribution function of the underlying signal
#'   distribution. By default, uses a `normal(+/-dprime/2, 1)` distribution.
#' @param lccdf The log complement cumulative distribution function of the
#'   underlying signal distribution. By default, uses a `normal(+/-dprime/2, 1)`
#'   distribution.
#' @param mean_M_0,sd_M_0,sd_M_1 Response-specific values of
#'   the mean and standard deviation of metacognitive efficiency.
#'   Ignored if `response_specific=FALSE` (default).
#' @param response_specific if `FALSE` (default), uses the same metacognitive
#'   efficiency for both Type I responses. If `TRUE`, uses response-specific
#'   metacognitive efficiencies for `"0"` and `"1"` responses.
#' @returns A simulated dataset of type 1 responses and confidence ratings, with
#'   columns:
#'   * `trial`: the simulated trial number
#'   * `participant`: the simulated participant number
#'   * `stimulus`: the value of the stimulus on each trial (either `0` or `1`)
#'   * `response`: the simulated type 1 response (either `0` or `1`)
#'   * `correct`: whether `stimulus==response` (either `0` or `1`)
#'   * `confidence`: the simulated type 2 response (from `1` to `length(c2_0_diff)+1`)
#'   * `dprime:theta_2`: the simulated agent's parameter values
#'
#'   If `summarize=TRUE`, the `trial` column is replaced with an `n` column
#'   indicating the number of simulated type 1/type 2 responses for each
#'   possible value.
#' @examples
#' sim_metad_participant(N_participants = 10, N_trials = 10)
#' sim_metad_participant(N_participants = 25, mean_dprime = 2, mean_M = -1)
#' @export
sim_metad_participant <- function(
  N_participants = 100, N_trials = 100,
  mean_dprime = 1, sd_dprime = .5, mean_c = 0, sd_c = .5,
  mean_M = 0, sd_M = .5,
  mean_z_c2_0 = rep(-1, 3), sd_z_c2_0 = rep(.1, 3), r_z_c2_0 = diag(3),
  mean_z_c2_1 = rep(-1, 3), sd_z_c2_1 = rep(.1, 3), r_z_c2_1 = diag(3),
  metac_absolute = TRUE, allow_negative_values = FALSE, summarize = FALSE,
  lcdf = normal_lcdf, lccdf = normal_lccdf,
  mean_M_0 = 0, sd_M_0 = .5, mean_M_1 = 0, sd_M_1 = .5, response_specific=FALSE
) {
  check_installed("purrr", reason = "to use `purrr::map` and `purrr::pmap`")

  # setup response-specific M-ratio
  if (!response_specific) {
    mean_M_0 <- mean_M
    mean_M_1 <- mean_M
    sd_M_0 <- sd_M
    sd_M_1 <- sd_M
  }
  
  d <- tidyr::expand_grid(participant = 1:N_participants) |>
    mutate(
      dprime = rnorm(n(), mean_dprime, sd_dprime),
      c = rnorm(n(), mean_c, sd_c),
      M_0 = rnorm(n(), mean_M_0, sd_M_0),
      M_1 = ifelse(
        rep(response_specific, n()),
        rnorm(n(), mean_M_1, sd_M_1),
        M_0
      ),
      c2_0_diff = purrr::map(
        .data$participant,
        ~ exp(rmulti_normal(1,
          mu = mean_z_c2_0,
          Sigma = cov_matrix(sd_z_c2_0, r_z_c2_0)
        ))
      ),
      c2_1_diff = purrr::map(
        .data$participant,
        ~ exp(rmulti_normal(1,
          mu = mean_z_c2_1,
          Sigma = cov_matrix(sd_z_c2_1, r_z_c2_1)
        ))
      )
    )

  if (!allow_negative_values) {
    d$M_0 <- exp(d$M_0)
    d$M_1 <- exp(d$M_1)
  }

  d |>
    mutate(data = purrr::pmap(
      list(
        .data$dprime, .data$c, .data$M_0, .data$M_1,
        .data$c2_0_diff, .data$c2_1_diff
      ),
      function(d, c , m0, m1, c2_0, c2_1) sim_metad(
        N = N_trials, dprime = d, c = c,
        M_0 = m0, M_1 = m1,
        c2_0_diff = c2_0, c2_1_diff = c2_1,
        metac_absolute = metac_absolute, summarize = summarize,
        lcdf = lcdf, lccdf = lccdf,
        response_specific=TRUE
      )
    )) |>
    select("participant", "data") |>
    tidyr::unnest("data")
}

#' Simulate from the hierarchical meta-d' model across within-participant
#' conditions
#'
#' @description Generate a simulated dataset across participants and conditions
#'   from the meta-d' model with sensitivity `dprime`, response bias `c`,
#'   metacognitive efficiency `M`, and distances between confidence thresholds
#'   `c2_0_diff` and `c2_1_diff` (for the two responses).
#' @param N_trials,N_participants Total number of participants and trials to
#'   simulate per participant. Half of these trials will have `stimulus=0` and
#'   half will have `stimulus=1`.
#' @param mean_dprime,sd_dprime,r_dprime The mean, standard deviation, and
#'   within-participant correlations of sensitivities of the signal detection
#'   agents to simulate
#' @param mean_c,sd_c,r_c The mean, standard deviation, and within-participant
#'   correlations of response bias of the signal detection agents to simulate
#' @param mean_M,sd_M,r_M The mean, standard deviation, and within-participant
#'   correlations of metacognitive efficiency of the agents. If
#'   `allow_negative_values==TRUE` (default), M-ratio is simulated on the
#'   logarithmic scale, where `0` indicates optimal metacognitive sensitivity,
#'   negative numbers indicate metacognitive inefficiency, and positive numbers
#'   indicate metacognitive hyper-efficiency. Otherwise, M-ratio is modeled on
#'   its natural scale to allow negative values.
#' @param mean_z_c2_0,mean_z_c2_1 Mean distance between confidence thresholds for
#'   `"0"` and `"1"` responses on the log_scale, such that `meta_c2_0 = meta_c -
#'   cumulative_sum(exp(z_c2_0))` and `meta_c2_1 = meta_c +
#'   cumulative_sum(exp(z_c2_1))`.
#' @param sd_z_c2_0_condition,sd_z_c2_1_condition SD of log distances across
#'   conditions between confidence thresholds for `"0"` and `"1"` responses on
#'   the log_scale.
#' @param sd_z_c2_0_confidence,sd_z_c2_1_confidence SD of log distances across
#'   confidence levels between confidence thresholds for `"0"` and `"1"`
#'   responses on the log_scale.
#' @param r_z_c2_0_condition,r_z_c2_1_condition Correlation across conditions of
#'   log distances between confidence thresholds for `"0"` and `"1"` responses
#'   on the log_scale.
#' @param r_z_c2_0_confidence,r_z_c2_1_confidence Correlation across confidence
#'   levels of log distances between confidence thresholds for `"0"` and `"1"`
#'   responses on the log_scale.
#' @param metac_absolute Determines how to fix the type 1 threshold for modeling
#'   confidence ratings. If `metac_absolute=TRUE`, `meta_c = c`. Otherwise,
#'   `meta_c = M * c`.
#' @param allow_negative_values If `allow_negative_values=FALSE` (default),
#'   M-ratio is simulated using a normal distribution on the logarithmic scale
#'   to prohibit negative values. If `allow_negative_values=TRUE`, then M-ratio
#'   is simulated on its natural scale, allowing negative values.
#' @param summarize Aggregate the data? If `summarize=FALSE`, returns a dataset
#'   with one row per observation. If `summarize=TRUE`, returns an aggregated
#'   dataset where `n` is the number of observations per response, accuracy, and
#'   confidence level.
#' @param lcdf The log cumulative distribution function of the underlying signal
#'   distribution. By default, uses a `normal(+/- dprime/2, 1)` distribution.
#' @param lccdf The log complement cumulative distribution function of the
#'   underlying signal distribution. By default, uses a `normal(+/- dprime/2,
#'   1)` distribution.
#' @param mean_M_0,mean_M_1,sd_M_0,sd_M_1,r_M_0,r_M_1 Response-specific values of
#'   the mean, standard deviation, and correlation across conditions of metacognitive
#'   efficiency. Ignored if `response_specific=FALSE` (default).
#' @param response_specific if `FALSE` (default), uses the same metacognitive
#'   efficiency for both Type I responses. If `TRUE`, uses response-specific
#'   metacognitive efficiencies for `"0"` and `"1"` responses.
#' @returns A simulated dataset of type 1 responses and confidence ratings, with
#'   columns:
#'   * `trial`: the simulated trial number
#'   * `stimulus`: the value of the stimulus on each trial (either `0` or `1`)
#'   * `response`: the simulated type 1 response (either `0` or `1`)
#'   * `correct`: whether `stimulus==response` (either `0` or `1`)
#'   * `confidence`: the simulated type 2 response (from `1` to `length(c2_0_diff)+1`)
#'   * `dprime:theta_2`: the simulated agent's parameter values
#'   If `summarize=TRUE`, the `trial` column is replaced with an `n` column
#'   indicating the number of simulated type 1/type 2 responses for each
#'   possible value.
#' @examples
#' sim_metad_participant_condition(10, 10)
#'
#' @export
sim_metad_participant_condition <- function(
  N_participants = 100, N_trials = 100,
  mean_dprime = rep(1, 2), sd_dprime = rep(.5, 2), r_dprime = diag(2),
  mean_c = rep(0, 2), sd_c = rep(.5, 2), r_c = diag(2),
  mean_M = rep(0, 2), sd_M = rep(.5, 2), r_M = diag(2),
  mean_z_c2_0 = matrix(rep(-1, 6), nrow = 3, ncol = 2),
  sd_z_c2_0_condition = rep(.1, 2), r_z_c2_0_condition = diag(2),
  sd_z_c2_0_confidence = rep(.1, 3), r_z_c2_0_confidence = diag(3),
  mean_z_c2_1 = matrix(rep(-1, 6), nrow = 3, ncol = 2),
  sd_z_c2_1_condition = rep(.1, 2), r_z_c2_1_condition = diag(2),
  sd_z_c2_1_confidence = rep(.1, 3), r_z_c2_1_confidence = diag(3),
  metac_absolute = TRUE, allow_negative_values = FALSE, summarize = FALSE,
  lcdf = normal_lcdf, lccdf = normal_lccdf,
  mean_M_0 = rep(0, 2), sd_M_0 = rep(.5, 2), r_M_0 = diag(2),
  mean_M_1 = rep(0, 2), sd_M_1 = rep(.5, 2), r_M_1 = diag(2),
  response_specific=FALSE
) {
  check_installed("purrr", reason = "to use `purrr::map_dbl`, `purrr::map` and `purrr::pmap`")

  ## calculate covariance matrices
  sigma_dprime <- cov_matrix(sd_dprime, r_dprime)
  sigma_c <- cov_matrix(sd_c, r_c)
  L_sigma_z_c2_0_condition <- chol(cov_matrix(sd_z_c2_0_condition, r_z_c2_0_condition))
  L_sigma_z_c2_0_confidence <- chol(cov_matrix(sd_z_c2_0_confidence, r_z_c2_0_confidence))
  L_sigma_z_c2_1_condition <- chol(cov_matrix(sd_z_c2_1_condition, r_z_c2_1_condition))
  L_sigma_z_c2_1_confidence <- chol(cov_matrix(sd_z_c2_1_confidence, r_z_c2_1_confidence))

  # setup response-specific M-ratios
  if (!response_specific) {
    mean_M_0 <- mean_M
    mean_M_1 <- mean_M
    sd_M_0 <- sd_M
    sd_M_1 <- sd_M
  }
  sigma_M_0 <- cov_matrix(sd_M_0, r_M_0)
  sigma_M_1 <- cov_matrix(sd_M_1, r_M_1)

  # helper function to select columns of a multivariate variable X
  mapmv <- function(column_index, X, map_fn=purrr::map_dbl) {
    map_fn(column_index, function(i, param) param[, i], X)
  }
  
  d <- tidyr::expand_grid(
    participant = 1:N_participants,
    condition = seq_along(mean_dprime)
  ) |>
    group_by(.data$participant) |>
    mutate(
      dprime = mapmv(.data$condition, rmulti_normal(1, mean_dprime, sigma_dprime)),
      c = mapmv(.data$condition, rmulti_normal(1, mean_c, sigma_c)),
      M_0 = mapmv(.data$condition, rmulti_normal(1, mean_M_0, sigma_M_0)),
      M_1 = ifelse(
        rep(response_specific, n()),
        mapmv(.data$condition, rmulti_normal(1, mean_M_1, sigma_M_1)),
        M_0
      ),
      c2_0_diff = mapmv(
        .data$condition,
        exp(rmatrixnorm(
          mean_z_c2_0,
          L_sigma_z_c2_0_confidence,
          L_sigma_z_c2_0_condition
        )),
        map_fn=purrr::map
      ),
      c2_1_diff = mapmv(
        .data$condition,
        exp(rmatrixnorm(
          mean_z_c2_1,
          L_sigma_z_c2_1_confidence,
          L_sigma_z_c2_1_condition
        )),
        map_fn=purrr::map
      )
    ) |>
    ungroup()
  
  if (!allow_negative_values) {
    d$M_0 <- exp(d$M_0)
    d$M_1 <- exp(d$M_1)
  }

  d |>
    mutate(data = purrr::pmap(
      list(
        .data$dprime, .data$c, .data$M_0, .data$M_1,
        .data$c2_0_diff, .data$c2_1_diff
      ),
      function(d, c , m0, m1, c2_0, c2_1) sim_metad(
        N = N_trials, dprime = d, c = c,
        M_0 = m0, M_1 = m1,
        c2_0_diff = c2_0, c2_1_diff = c2_1,
        metac_absolute = metac_absolute, summarize = summarize,
        lcdf = lcdf, lccdf = lccdf,
        response_specific=TRUE
      )
    )) |>
    select("participant", "condition", "data") |>
    tidyr::unnest("data")
}

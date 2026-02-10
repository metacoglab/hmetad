#' Generate Stan code for the meta-d' model
#'
#' @param K The number of confidence levels
#' @param distribution The noise distribution to use. Should be a parameter-free
#' distribution, i.e., one that is mean-centered without additional variance/shape parameters.
#' If the distribution is not already available in stan, you must additionally provide two
#' functions to Stan (one for `<distribution>_lcdf` and one for `<distribution>_lccdf`).
#' @param metac_absolute Should the type 2 criterion (metac) be fixed to the absolute type 1 criterion (c)?
#' If `TRUE`, the model will set `metac = c`. Otherwise, it will set `metac = M * c`, such that
#' the type 2 criterion is _relatively_ equal to the type 1 criterion
#' (i.e., `meta_c/meta_dprime = c/dprime`)
#' @returns A single string containing Stan code defining the likelihood for the metad' model
#' with `K` confidence levels, signal distributed according to the distribution `distribution`,
#' and where `metac = c` if `metac_absolute==TRUE`, and `metac = M*c` otherwise.
#' @keywords internal
stancode_metad <- function(K, distribution = "normal", metac_absolute = TRUE) {
  if (!is.numeric(K) || K <= 1) {
    stop("Number of confidence levels must be an integer greater than 1")
  }
  k <- K - 1

  dist_fun <- function(x, mean, fun = "lcdf") {
    if (distribution == "normal") {
      paste0("std_normal_", fun, "(", x, " - ", mean, ")")
    } else {
      paste0(distribution, "_", fun, "(", x, " | ", mean, ")")
    }
  }

  paste0(
    "	// Convert a binary int x from {0, 1} to {-1, 1}
	int to_signed(int x) {
	  return 2*x - 1;
	}

	// P(response, confidence | stimulus) given as simplex
	// [P(resp=0, conf=K), .... P(resp=0, conf=1), P(resp=1, conf=1), ... P(resp=1, conf=K)]
	vector metad_", distribution, "_pmf(int stimulus, real dprime, real c, real meta_dprime, real meta_c, vector meta_c2_0, vector meta_c2_1) {
		// number of confidence levels
		int K = size(meta_c2_0)+1;

  	// type-1 response probabilities
	  real lp_1 = ", dist_fun("c", "to_signed(stimulus)*dprime/2", "lccdf"), ";
  	real lp_0 = ", dist_fun("c", "to_signed(stimulus)*dprime/2", "lcdf"), ";

  	// means of type-2 distributions
  	real meta_mu = to_signed(stimulus) * meta_dprime/2;

	  vector[K] lp2_1;         // CDFs (response == 1)
  	vector[K] lp2_0;         // CDFs (response == 0)
		vector[2*K] log_theta;   // joint (type-1 x type-2) response probabilities

	  lp2_1[1] = ", dist_fun("meta_c", "meta_mu", "lccdf"), ";
  	lp2_0[1] = ", dist_fun("meta_c", "meta_mu", "lcdf"), ";
  	for (k in 2:K) {
    	lp2_1[k] = ", dist_fun("meta_c2_1[k-1]", "meta_mu", "lccdf"), ";
    	lp2_0[k] = ", dist_fun("meta_c2_0[k-1]", "meta_mu", "lcdf"), ";

			log_theta[K-k+2] = log_diff_exp(lp2_0[k-1], lp2_0[k]);
    	log_theta[K+k-1] = log_diff_exp(lp2_1[k-1], lp2_1[k]);
  	}
  	log_theta[1] = lp2_0[K];
  	log_theta[2*K] = lp2_1[K];

	  // weight by P(response|stimulus) and normalize
  	log_theta[1:K] += lp_0 - lp2_0[1];
  	log_theta[(K+1):(2*K)] += lp_1 - lp2_1[1];

	  return exp(log_theta);
	}

	real metad__", K, "__", distribution, "__", ifelse(metac_absolute, "absolute", "relative"),
    "_lpmf(array[] int Y, real M, real dprime, real c, ",
    paste0("real z_meta_c2_0_", 1:k, collapse = ", "), ", ",
    paste0("real z_meta_c2_1_", 1:k, collapse = ", "),
    ") {
		int K = size(Y) %/% 4; // number of confidence levels

		real meta_dprime = M * dprime;
		real meta_c = ", ifelse(metac_absolute, "c", "M * c"), ";
		vector[K-1] meta_c2_0 = meta_c - cumulative_sum([",
    paste0("z_meta_c2_0_", 1:k, collapse = ", "),
    "]');
		vector[K-1] meta_c2_1 = meta_c + cumulative_sum([",
    paste0("z_meta_c2_1_", 1:k, collapse = ", "),
    "]');

		// use multinomial likelihood
		return multinomial_lpmf(Y[1:(2*K)] | metad_", distribution, "_pmf(0, dprime, c,
														meta_dprime, meta_c, meta_c2_0, meta_c2_1)) +
  		multinomial_lpmf(Y[(2*K+1):(4*K)] |  metad_", distribution, "_pmf(1, dprime, c,
											 meta_dprime, meta_c, meta_c2_0, meta_c2_1));
	}"
  )
}

#' Get the R function for the model's underlying distribution functions
#' @param model The `brms` model to get distribution functions for
#' @param fun The distribution function to return.
#' @returns An R function with the name `distribution_{fun}`.
#' @details Will throw an error if this function does not exist
#' @keywords internal
get_dist <- function(model, fun = "lcdf") {
  if (model$family$family != "custom" ||
    !stringr::str_starts(model$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }
  dist <- stringr::str_match(
    model$family$name,
    "metad__[[:digit:]]*__(.*)__[[:alpha:]]*"
  )[, 2]
  paste0(dist, "_", fun) |>
    sym() |>
    eval()
}

#' Get the parameterization of `meta_c` in `model`
#' @param model The `brms` model to get the parameterization for
#' @returns A character vector, either `"absolute"` or `"relative"`.
#' @keywords internal
get_metac <- function(model) {
  if (model$family$family != "custom" ||
    !stringr::str_starts(model$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }
  stringr::str_match(
    model$family$name,
    "metad__[[:digit:]]*__.*__([[:alpha:]]*)"
  )[, 2]
}

#' Normal cumulative distribution functions
#' @param x The quantile to evaluate the l(c)cdf at
#' @param mu The mean of the normal distribution
#' @returns \eqn{log(P(X < x))} (for `normal_lcdf`) or \eqn{log(P(X > x))} (for `normal_lccdf`) where \eqn{X} is sampled from a normal distribution
#' with mean `mu` and standard deviation of \eqn{1}
#' @examples
#' normal_lcdf(0, mu = 1)
#' normal_lccdf(0, mu = 1)
#'
#' @rdname normal_dist
#' @export
normal_lcdf <- function(x, mu) pnorm(x, mean = mu, log.p = TRUE)

#' @rdname normal_dist
#' @export
normal_lccdf <- function(x, mu) pnorm(x, mean = mu, log.p = TRUE, lower.tail = FALSE)

#' Generate (log) probability simplex over the joint type 1/type 2 responses
#' @param stimulus the stimulus (0 or 1)
#' @param dprime the type 1 sensitivity
#' @param c the type 1 response criterion
#' @param meta_dprime the type 2 sensitivity
#' @param meta_c the type 1 criteriom for generating confidence ratings
#' @param meta_c2_0 the type 2 response criteria for `"0"` responses, indexed by
#' increasing confidence levels
#' @param meta_c2_1 the type 2 response criteria for `"1"` responses, indexed by
#' increasing confidence levels
#' @param lcdf The log cumulative distribution function for the underlying distribution in the metad' model.
#' By default, uses the normal distribution with a standard deviation of `1`.
#' @param lccdf The log complement cumulative distribution function for the underlying distribution in the metad' model.
#' By default, uses the normal distribution with a standard deviation of `1`.
#' @param log if TRUE, return log probabilities instead of probabilities
#' @returns A probability simplex
#' \deqn{\begin{bmatrix} P(R=0, C=K \vert S=0), \ldots, P(R=0, C=1 \vert S=0),
#' P(R=0, C=1 \vert S=1), \ldots, P(R=1, C=1 \vert S=1)\end{bmatrix}}
#' for response \eqn{R} and confidence \eqn{C} given stimulus \eqn{S},
#' as defined by the meta-d' model.
#' @examples
#' metad_pmf(
#'   stimulus = 0, dprime = 2, c = .5, meta_dprime = 1, meta_c = .5,
#'   meta_c2_0 = c(0, -.5), meta_c2_1 = c(1, 1.5)
#' )
#' @export
metad_pmf <- function(stimulus, dprime, c,
                      meta_dprime, meta_c,
                      meta_c2_0, meta_c2_1,
                      lcdf = normal_lcdf, lccdf = normal_lccdf,
                      log = FALSE) {
  if (!is.numeric(stimulus) || stimulus < 0 || stimulus > 1 ||
    (stimulus > 0 && stimulus < 1)) {
    stop(paste0("Stimulus should be `0` or `1`, but is ", stimulus))
  }
  if (!all(
    length(dprime) == 1, length(c) == 1, length(meta_dprime) == 1,
    is.numeric(dprime), is.numeric(c), is.numeric(meta_dprime)
  )) {
    stop("Error: `dprime`, `c`, and `meta_dprime` must be single numbers.")
  }
  if (!is.numeric(meta_c2_0) || !is.numeric(meta_c2_1) ||
    length(meta_c2_0) != length(meta_c2_1) ||
    !all(diff(c(meta_c, meta_c2_0)) < 0) ||
    !all(diff(c(meta_c, meta_c2_1)) > 0)) {
    stop("Error: `meta_c2_0` and meta_c2_1` must be ordered vectors of the same length constrained by `meta_c`.")
  }

  # number of confidence levels
  K <- length(meta_c2_0) + 1

  # type-1 response probabilities
  lp_1 <- lccdf(c, to_signed(stimulus) * dprime / 2)
  lp_0 <- lcdf(c, to_signed(stimulus) * dprime / 2)

  # calculate normal cdfs (log scale)
  lp2_1 <- lccdf(c(meta_c, meta_c2_1), to_signed(stimulus) * meta_dprime / 2)
  lp2_0 <- lcdf(c(meta_c, meta_c2_0), to_signed(stimulus) * meta_dprime / 2)

  # response probabilities
  log_theta <- rep(0, 2 * K)
  for (k in 1:(K - 1)) {
    log_theta[K - k + 1] <- log(exp(lp2_0[k]) - exp(lp2_0[k + 1]))
    log_theta[K + k] <- log(exp(lp2_1[k]) - exp(lp2_1[k + 1]))
  }
  log_theta[1] <- lp2_0[K]
  log_theta[2 * K] <- lp2_1[K]

  # weight by P(response|stimulus) and normalize
  log_theta[1:K] <- log_theta[1:K] + lp_0 - lp2_0[1]
  log_theta[(K + 1):(2 * K)] <- log_theta[(K + 1):(2 * K)] + lp_1 - lp2_1[1]

  if (log) {
    log_theta
  } else {
    exp(log_theta)
  }
}

#' Generate posterior predictions for the metad' model
#' @param prep an object containing the data and model draws
#' @returns A `[D x N x K*4]` array containing posterior samples of
#' the joint probability of a type 1/type 2 response,
#' where `D` is the number of posterior draws,
#' `N` is the number of rows in the data, and
#' `K` is the number of confidence levels.
#' @keywords internal
posterior_epred_metad <- function(prep) {
  M <- brms::get_dpar(prep, "mu")
  dprime <- brms::get_dpar(prep, "dprime")
  c1 <- brms::get_dpar(prep, "c")

  # align dimensions
  n_obs <- dim(M)[2]
  if (is.vector(dprime)) {
    dprime <- replicate(n_obs, dprime)
  }
  if (is.vector(c1)) {
    c1 <- replicate(n_obs, c1)
  }
  meta_dprime <- M * dprime
  meta_c <- NULL
  if (get_metac(prep) == "absolute") {
    meta_c <- c1
  } else {
    meta_c <- M * c1
  }

  # determine confidence thresholds
  dpars <- names(prep$dpars)
  meta_c2_0 <- NULL
  meta_c2_1 <- NULL

  if (is.vector(brms::get_dpar(prep, "metac2zero1diff"))) {
    if (length(dpars[stringr::str_detect(dpars, "metac2zero")]) == 1) {
      meta_c2_0 <- brms::get_dpar(prep, "metac2zero1diff")
      meta_c2_0 <- array(meta_c2_0, dim = c(length(meta_c2_0), 1, 1))
    } else {
      meta_c2_0 <- dpars[stringr::str_detect(dpars, "metac2zero")] |>
        sapply(function(s) brms::get_dpar(prep, s)) |>
        apply(1, cumsum) |>
        t() |>
        replicate(last(dim(meta_c)), expr = _) |>
        aperm(c(1, 3, 2))
    }
  } else {
    meta_c2_0 <- dpars[stringr::str_detect(dpars, "metac2zero")] |>
      lapply(function(s) brms::get_dpar(prep, s)) |>
      abind::abind(along = 3) |>
      apply(1:2, cumsum) |>
      aperm(c(2, 3, 1))
  }

  if (is.vector(brms::get_dpar(prep, "metac2one1diff"))) {
    if (length(dpars[stringr::str_detect(dpars, "metac2one")]) == 1) {
      meta_c2_1 <- brms::get_dpar(prep, "metac2one1diff")
      meta_c2_1 <- array(meta_c2_1, dim = c(length(meta_c2_1), 1, 1))
    } else {
      meta_c2_1 <- dpars[stringr::str_detect(dpars, "metac2one")] |>
        sapply(function(s) brms::get_dpar(prep, s)) |>
        apply(1, cumsum) |>
        t() |>
        replicate(last(dim(meta_c)), expr = _) |>
        aperm(c(1, 3, 2))
    }
  } else {
    meta_c2_1 <- dpars[stringr::str_detect(dpars, "metac2one")] |>
      lapply(function(s) brms::get_dpar(prep, s)) |>
      abind::abind(along = 3) |>
      apply(1:2, cumsum) |>
      aperm(c(2, 3, 1))
  }

  # calculate number of confidence thresholds
  k <- last(dim(meta_c2_0))
  K <- k + 1

  # calculate confidence threhsolds
  meta_c2_0 <- replicate(k, meta_c) - meta_c2_0
  meta_c2_1 <- replicate(k, meta_c) + meta_c2_1


  # calculate joint response & confidence probabilities
  lcdf <- get_dist(prep, fun = "lcdf")
  lccdf <- get_dist(prep, fun = "lccdf")
  p <- array(dim = c(dim(dprime), 4 * K))
  for (s in 1:first(dim(dprime))) {
    for (i in 1:last(dim(dprime))) {
      p[s, i, 1:(2 * K)] <-
        metad_pmf(0, dprime[s, i], c1[s, i], meta_dprime[s, i],
          meta_c[s, i], meta_c2_0[s, i, ], meta_c2_1[s, i, ],
          lcdf = lcdf, lccdf = lccdf
        )
      p[s, i, (2 * K + 1):(4 * K)] <-
        metad_pmf(1, dprime[s, i], c1[s, i], meta_dprime[s, i],
          meta_c[s, i], meta_c2_0[s, i, ], meta_c2_1[s, i, ],
          lcdf = lcdf, lccdf = lccdf
        )
    }
  }

  p
}

#' Calculate the log probability simplex of the metad' model
#' @param i an observation index
#' @param prep an object containing the data and model draws
#' @returns A vector of joint type 1/type 2 response probabilties
#' for observation `i` in `prep`
#' @keywords internal
lp_metad <- function(i, prep) {
  M <- brms::get_dpar(prep, "mu", i = i)
  dprime <- brms::get_dpar(prep, "dprime", i = i)
  c1 <- brms::get_dpar(prep, "c", i = i)
  meta_dprime <- M * dprime
  meta_c <- NULL
  if (get_metac(prep) == "absolute") {
    meta_c <- c1
  } else {
    meta_c <- M * c1
  }

  # determine confidence thresholds
  dpars <- names(prep$dpars)
  meta_c2_0 <- dpars[stringr::str_detect(dpars, "metac2zero")] |>
    sapply(function(s) brms::get_dpar(prep, s, i = i))
  if (is.vector(meta_c2_0)) {
    meta_c2_0 <- matrix(meta_c2_0, ncol = length(meta_c2_0))
  }
  meta_c2_0 <- meta_c2_0 |>
    apply(1, cumsum) |>
    t()
  meta_c2_1 <- dpars[stringr::str_detect(dpars, "metac2one")] |>
    sapply(function(s) brms::get_dpar(prep, s, i = i))
  if (is.vector(meta_c2_1)) {
    meta_c2_1 <- matrix(meta_c2_1, ncol = length(meta_c2_1))
  }
  meta_c2_1 <- meta_c2_1 |>
    apply(1, cumsum) |>
    t()
  meta_c2_0 <- meta_c - meta_c2_0
  meta_c2_1 <- meta_c + meta_c2_1
  meta_c2_0 <- split(meta_c2_0, row(meta_c2_0))
  meta_c2_1 <- split(meta_c2_1, row(meta_c2_1))

  # calculate joint response & confidence probabilities
  lcdf <- get_dist(prep, fun = "lcdf")
  lccdf <- get_dist(prep, fun = "lccdf")
  PMF <- Vectorize(metad_pmf,
    vectorize.args = c(
      "stimulus", "dprime", "c", "meta_dprime",
      "meta_c", "meta_c2_0", "meta_c2_1"
    )
  )
  lp_0 <- PMF(0, dprime, c1, meta_dprime, meta_c, meta_c2_0, meta_c2_1,
    log = TRUE, lcdf = lcdf, lccdf = lccdf
  )
  lp_1 <- PMF(1, dprime, c1, meta_dprime, meta_c, meta_c2_0, meta_c2_1,
    log = TRUE, lcdf = lcdf, lccdf = lccdf
  )

  t(rbind(lp_0, lp_1))
}

#' Generate a function to calculate the log likelihood of the metad' model
#' @param i an observation index
#' @param prep an object containing the data and model draws
#' @returns A `[D x K*4]` array containing posterior samples of
#' the joint probability of a type 1/type 2 response,
#' where `D` is the number of posterior draws,
#' `N` is the number of rows in the data, and
#' `K` is the number of confidence levels.
#' @keywords internal
log_lik_metad <- function(i, prep) {
  p <- exp(lp_metad(i, prep))

  if (any(is.na(prep$data$Y))) {
    stop("Error: please provide sample data y with trial counts")
  }

  y <- prep$data$Y[i, ]
  N_0 <- sum(y[1:(length(y) / 2)])
  N_1 <- sum(y[(length(y) / 2 + 1):length(y)])

  # calculate multinomial response probabilities
  apply(
    p[, 1:(ncol(p) / 2), drop = FALSE], 1,
    function(prob) {
      dmultinom(y[1:(length(y) / 2)],
        size = N_0, prob = prob, log = TRUE
      )
    }
  ) +
    apply(
      p[, (ncol(p) / 2 + 1):ncol(p), drop = FALSE], 1,
      function(prob) {
        dmultinom(y[(length(y) / 2 + 1):length(y)],
          size = N_1, prob = prob, log = TRUE
        )
      }
    )
}

#' Simulate posterior predictions from the metad' model
#' @param i an observation index
#' @param prep an object containing the data and model draws
#' @param ... Additional arguments. Not currently used.
#' @returns A `[D x K*4]` array containing posterior samples of
#' counts of joint type 1/type 2 responses,
#' where `D` is the number of posterior draws,
#' `N` is the number of rows in the data, and
#' `K` is the number of confidence levels.
#' @keywords internal
posterior_predict_metad <- function(i, prep, ...) {
  p <- exp(lp_metad(i, prep))

  if (any(is.na(prep$data$Y))) {
    stop("Error: please provide sample data y with trial counts")
  }

  y <- prep$data$Y[i, ]
  N_0 <- as.integer(sum(y[1:(length(y) / 2)]))
  N_1 <- as.integer(sum(y[(length(y) / 2 + 1):length(y)]))

  # simulate from a multinomial distribution
  rbind(
    apply(p[, 1:(ncol(p) / 2), drop = FALSE], 1, rmultinom, n = 1, size = N_0),
    apply(p[, (ncol(p) / 2 + 1):ncol(p), drop = FALSE], 1, rmultinom, n = 1, size = N_1)
  ) |>
    t()
}

#' `brms` family for the metad' model
#' @param K The number of confidence levels
#' @param distribution The noise distribution to use for the signal detection model
#' @param metac_absolute If `TRUE`, fix the type 2 criterion to be equal to the type 1 criterion.
#' Otherwise, equate the criteria relatively such that
#' \deqn{\frac{\textrm{meta-}c}{\textrm{meta-}d'} = \frac{c}{d'}}
#' @returns A `brms` family for the metad' model with \eqn{K} confidence levels
#' @examples
#' # create a family using the normal distribution and 3 levels of confidence
#' metad(3)
#'
#' # create a family with meta_c = M * c
#' metad(3, metac_absolute = FALSE)
#'
#' # create a family with an alternative distribution
#' # note: cumulative distribution functions must be defined
#' # in R and in Stan using [brms::stanvar()]
#' metad(4, distribution = "gumbel_min")
#' @export
metad <- function(K, distribution = "normal", metac_absolute = TRUE) {
  k <- K - 1
  brms::custom_family(
    name = paste0(
      "metad__", K, "__", distribution, "__",
      ifelse(metac_absolute, "absolute", "relative")
    ),
    dpars = c(
      "mu", "dprime", "c", paste0("metac2zero", 1:k, "diff"),
      paste0("metac2one", 1:k, "diff")
    ),
    links = c("log", "identity", "identity", rep("log", 2 * k)),
    lb = c(0, NA, NA, rep(0, 2 * k)),
    type = "int", specials = c("multinomial"),
    log_lik = log_lik_metad,
    posterior_predict = posterior_predict_metad,
    posterior_epred = posterior_epred_metad
  )
}

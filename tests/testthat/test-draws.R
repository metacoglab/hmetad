library(tidyr)
library(tidybayes)

d <- sim_metad(
  N_trials = 1000000, dprime = .75, c = -.5, log_M = -1,
  c2_0 = c(.25, .75, 1), c2_1 = c(.5, 1, 1.25)
)
m <- fit_metad(N ~ 1, d)
newdata <- tibble(.row = 1)

test_that("linpred_draws_metad works", {
  m |>
    linpred_draws_metad(newdata, pivot_longer = TRUE) |>
    median_qi() |>
    nrow() |>
    expect_equal(11)

  m |>
    linpred_draws_metad(newdata) |>
    median_qi(M) |>
    nrow() |>
    expect_equal(1)

  expect_equal(
    linpred_draws_metad(m, newdata),
    add_linpred_draws_metad(newdata, m)
  )

  m |>
    linpred_rvars_metad(newdata, pivot_longer = TRUE) |>
    median_qi() |>
    nrow() |>
    expect_equal(11)

  m |>
    linpred_rvars_metad(newdata) |>
    nrow() |>
    expect_equal(1)

  expect_equal(
    linpred_rvars_metad(m, newdata),
    add_linpred_rvars_metad(newdata, m)
  )

  ## compare between _draws and _rvars
  linpred_draws_metad(m, newdata, pivot_longer = TRUE) |>
    median_qi() |>
    arrange(.variable) |>
    pull(.value) |>
    near(
      linpred_rvars_metad(m, newdata, pivot_longer = TRUE) |>
        median_qi() |>
        arrange(.variable) |>
        pull(.value),
      tol = .01
    ) |>
    expect_all_true()
})

test_that("epred_draws_metad works", {
  ## compare epred to empirical joint response probabilities
  epred_draws_metad(m, newdata) |>
    median_qi() |>
    arrange(stimulus, response, confidence) |>
    pull(.epred) |>
    near(
      count(d) |>
        group_by(stimulus) |>
        mutate(p = n / sum(n)) |>
        pull(p),
      tol = 0.01
    ) |>
    expect_all_true()

  ## compare _draws and _rvars
  epred_draws_metad(m, newdata) |>
    median_qi() |>
    expect_equal(
      epred_rvars_metad(m, newdata) |>
        median_qi()
    )
})

test_that("predicted_draws_metad works", {
  ## compare predictions to empirical joint response probabilities
  predicted_draws_metad(m, m$data) |>
    group_by(.row, stimulus, .chain, .iteration, .draw) |>
    mutate(.prediction = .prediction / sum(.prediction)) |>
    group_by(.row, stimulus, response, confidence) |>
    median_qi(.prediction) |>
    arrange(stimulus, response, confidence) |>
    pull(.prediction) |>
    near(
      count(d) |>
        group_by(stimulus) |>
        mutate(p = n / sum(n)) |>
        pull(p),
      tol = 0.01
    ) |>
    expect_all_true()

  ## compare predictions to empirical joint response probabilities
  predicted_rvars_metad(m, m$data) |>
    group_by(.row, stimulus) |>
    mutate(.prediction = .prediction / rvar_sum(.prediction)) |>
    group_by(.row, stimulus, response, confidence) |>
    median_qi(.prediction) |>
    arrange(stimulus, response, confidence) |>
    pull(.prediction) |>
    near(
      count(d) |>
        group_by(stimulus) |>
        mutate(p = n / sum(n)) |>
        pull(p),
      tol = 0.01
    ) |>
    expect_all_true()
})

test_that("mean_confidence_draws works", {
  m |>
    mean_confidence_draws(newdata) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(stimulus, response) |>
        summarize(m = mean(confidence), .groups = "keep") |>
        pull(m),
      tol = .01
    ) |>
    expect_all_true()

  m |>
    mean_confidence_draws(newdata, by_stimulus = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(response) |>
        summarize(m = mean(confidence), .groups = "keep") |>
        pull(m),
      tol = .01
    ) |>
    expect_all_true()

  m |>
    mean_confidence_draws(newdata, by_response = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(stimulus) |>
        summarize(m = mean(confidence), .groups = "keep") |>
        pull(m),
      tol = .01
    ) |>
    expect_all_true()

  m |>
    mean_confidence_draws(newdata, by_stimulus = FALSE, by_response = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by() |>
        summarize(m = mean(confidence), .groups = "keep") |>
        pull(m),
      tol = .01
    ) |>
    expect_all_true()

  expect_equal(
    mean_confidence_draws(m, newdata),
    add_mean_confidence_draws(newdata, m)
  )


  m |>
    mean_confidence_rvars(newdata) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(stimulus, response) |>
        summarize(m = mean(confidence), .groups = "keep") |>
        pull(m),
      tol = .01
    ) |>
    expect_all_true()

  m |>
    mean_confidence_rvars(newdata, by_stimulus = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(response) |>
        summarize(m = mean(confidence), .groups = "keep") |>
        pull(m),
      tol = .01
    ) |>
    expect_all_true()

  m |>
    mean_confidence_rvars(newdata, by_response = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(stimulus) |>
        summarize(m = mean(confidence), .groups = "keep") |>
        pull(m),
      tol = .01
    ) |>
    expect_all_true()

  m |>
    mean_confidence_rvars(newdata, by_stimulus = FALSE, by_response = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by() |>
        summarize(m = mean(confidence), .groups = "keep") |>
        pull(m),
      tol = .01
    ) |>
    expect_all_true()

  expect_equal(
    mean_confidence_rvars(m, newdata),
    add_mean_confidence_rvars(newdata, m)
  )
})


test_that("metacognitive_bias_draws works", {
  draws <- metacognitive_bias_draws(m, newdata)

  expect_equal(nrow(median_qi(draws)), 2)

  draws |>
    mutate(test = metacognitive_bias > 0) |>
    pull(test) |>
    expect_all_true()

  expect_equal(draws, add_metacognitive_bias_draws(newdata, m))


  draws2 <- metacognitive_bias_rvars(m, newdata)

  expect_equal(nrow(median_qi(draws2)), 2)

  draws2 |>
    mutate(test = all(metacognitive_bias > 0)) |>
    pull(test) |>
    expect_all_true()

  expect_equal(draws2, add_metacognitive_bias_rvars(newdata, m))

  ## compare between _draws and _rvars
  draws |>
    median_qi() |>
    pull(metacognitive_bias) |>
    near(
      draws2 |>
        median_qi() |>
        pull(metacognitive_bias),
      tol = .01
    ) |>
    expect_all_true()
})


test_that("roc1_draws works", {
  draws <- roc1_draws(m, newdata)

  draws |>
    pull(p_hit) |>
    between(0, 1) |>
    expect_all_true()

  draws |>
    pull(p_fa) |>
    between(0, 1) |>
    expect_all_true()

  expect_equal(draws, add_roc1_draws(newdata, m))

  ## compare between _draws and _rvars
  draws |>
    median_qi(p_hit) |>
    pull(p_hit) |>
    near(
      roc1_rvars(m, newdata) |>
        median_qi(p_hit) |>
        pull(p_hit),
      tol = .01
    ) |>
    expect_all_true()

  draws |>
    median_qi(p_fa) |>
    pull(p_fa) |>
    near(
      roc1_rvars(m, newdata) |>
        median_qi(p_fa) |>
        pull(p_fa),
      tol = .01
    ) |>
    expect_all_true()
})

test_that("roc2_draws works", {
  draws <- roc2_draws(m, newdata)

  draws |>
    pull(p_hit2) |>
    between(0, 1) |>
    expect_all_true()

  draws |>
    pull(p_fa2) |>
    between(0, 1) |>
    expect_all_true()

  expect_equal(draws, add_roc2_draws(newdata, m))

  ## compare between _draws and _rvars
  draws |>
    median_qi(p_hit2) |>
    pull(p_hit2) |>
    near(
      roc2_rvars(m, newdata) |>
        median_qi(p_hit2) |>
        pull(p_hit2),
      tol = .01
    ) |>
    expect_all_true()

  draws |>
    median_qi(p_fa2) |>
    pull(p_fa2) |>
    near(
      roc2_rvars(m, newdata) |>
        median_qi(p_fa2) |>
        pull(p_fa2),
      tol = .01
    ) |>
    expect_all_true()
})

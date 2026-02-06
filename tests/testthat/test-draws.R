library(tidyr)
library(tidybayes)
print(getwd())

d <- sim_metad(
  N_trials = 1000000, dprime = .75, c = -.5, log_M = -1,
  c2_0 = c(.25, .75, 1), c2_1 = c(.5, 1, 1.25)
)
m <- fit_metad(N ~ 1, d)
newdata <- tibble(.row = 1)

test_that("mean_confidence_draws works", {
  m |>
    mean_confidence_draws(newdata) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(stimulus, response) |>
        summarize(m = mean(confidence)) |>
        pull(m),
      tol = .01
    ) |>
    all() |>
    expect_equal(TRUE)

  m |>
    mean_confidence_draws(newdata, by_stimulus = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(response) |>
        summarize(m = mean(confidence)) |>
        pull(m),
      tol = .01
    ) |>
    all() |>
    expect_equal(TRUE)

  m |>
    mean_confidence_draws(newdata, by_response = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(stimulus) |>
        summarize(m = mean(confidence)) |>
        pull(m),
      tol = .01
    ) |>
    all() |>
    expect_equal(TRUE)

  m |>
    mean_confidence_draws(newdata, by_stimulus = FALSE, by_response = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by() |>
        summarize(m = mean(confidence)) |>
        pull(m),
      tol = .01
    ) |>
    all() |>
    expect_equal(TRUE)
})


test_that("metacognitive_bias_draws works", {
  draws <- metacognitive_bias_draws(m, newdata)

  expect_equal(nrow(median_qi(draws)), 2)

  draws |>
    mutate(test = metacognitive_bias > 0) |>
    pull(test) |>
    all() |>
    expect_equal(TRUE)
})


test_that("roc1_draws works", {
  draws <- roc1_draws(m, newdata)

  draws |>
    pull(p_hit) |>
    between(0, 1) |>
    all() |>
    expect_equal(TRUE)

  draws |>
    pull(p_fa) |>
    between(0, 1) |>
    all() |>
    expect_equal(TRUE)
})

test_that("roc2_draws works", {
  draws <- roc2_draws(m, newdata)

  draws |>
    pull(p_hit2) |>
    between(0, 1) |>
    all() |>
    expect_equal(TRUE)

  draws |>
    pull(p_fa2) |>
    between(0, 1) |>
    all() |>
    expect_equal(TRUE)
})

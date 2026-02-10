test_that("cov_matrix works", {
  expect_equal(
    cov_matrix(rep(2, 2), matrix(c(1, .25, .25, 1), nrow = 2)),
    matrix(c(4, 1, 1, 4), nrow = 2)
  )

  # mismatched sizes
  expect_error(cov_matrix(1, 1))
  expect_error(cov_matrix(1, matrix(c(1, .5, .5, 1), nrow = 2)))

  # invalid S
  expect_error(cov_matrix(rep(-1, 2), matrix(c(1, .5, .5, 1), nrow = 2)))

  # invalid OMEGA
  expect_error(cov_matrix(rep(1, 2), matrix(c(1, 1.5, 1.5, 1), nrow = 2)))
})

test_that("cor_matrix works", {
  expect_equal(cor_matrix(1), matrix(rep(1, 4), nrow = 2))
  expect_equal(cor_matrix(0, nrow = 100), diag(100))

  # invalid correlations
  expect_error(cor_matrix(2))
  expect_error(cor_matrix(-2))

  # invalid nrow
  expect_error(cor_matrix(.5, nrow = 1))
})

test_that("rmatrixnorm works", {
  mu <- matrix(rep(0, 8), nrow = 4)
  sd_rows <- rep(1, 4)
  sd_cols <- rep(1, 2)
  r_rows <- cor_matrix(.25, 4)
  r_cols <- cor_matrix(.75, 2)
  L_sigma_rows <- chol(cov_matrix(sd_rows, r_rows))
  L_sigma_cols <- chol(cov_matrix(sd_cols, r_cols))
  rmatrixnorm(mu, L_sigma_rows, L_sigma_cols) |>
    dim() |>
    expect_equal(c(4, 2))

  # dimension mismatch
  expect_error(rmatrixnorm(mu[1:3, ], L_sigma_rows, L_sigma_cols))
})

test_that("sim_metad works", {
  expect_equal(nrow(sim_metad(N = 100)), 100)
})

test_that("sim_metad_condition works", {
  expect_equal(nrow(sim_metad_condition(N = 100)), 200)
})

test_that("sim_metad_participant works", {
  sim_metad_participant(N_trials = 100, N_participants = 10) |>
    nrow() |>
    expect_equal(1000)
})

test_that("sim_metad_participant_condition works", {
  sim_metad_participant_condition(N_trials = 100, N_participants = 10) |>
    nrow() |>
    expect_equal(2000)
})

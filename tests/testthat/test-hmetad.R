test_that("`to_unsigned` works", {
  expect_equal(to_unsigned(-1), 0)
  expect_equal(to_unsigned(1), 1)

  expect_equal(to_unsigned(-10), 0)
  expect_equal(to_unsigned(10), 1)
})

test_that("`to_signed` works", {
  expect_equal(to_signed(0), -1)
  expect_equal(to_signed(1), 1)

  expect_equal(to_signed(FALSE), -1)
  expect_equal(to_signed(TRUE), 1)
})


test_that("joint_response works", {
  expect_equal(
    joint_response(
      rep(c(0, 1), each = 4),
      c(4:1, 1:4),
      4
    ),
    1:8
  )
})

test_that("type1_response works", {
  expect_equal(
    type1_response(1:6, 3),
    rep(0:1, each = 3)
  )
})

test_that("type2_response works", {
  expect_equal(
    type2_response(1:6, 3),
    c(3:1, 1:3)
  )
})

test_that("response_probabilities works", {
  response_probabilities(rep(1, 8)) |>
    expect_equal(rep(1 / 4, 8))

  matrix(rep(1, 16), nrow = 2) |>
    response_probabilities() |>
    expect_equal(matrix(rep(1 / 4, 16), nrow = 2))
})

test_that("aggregate_metad works", {
  expect_equal(
    aggregate_metad(tibble(), K = 2),
    tibble(
      N_0 = 0, N_1 = 0,
      N = matrix(rep(0, 8),
        nrow = 1,
        dimnames = list(
          NULL,
          paste0(
            "N_", rep(0:1, each = 4),
            "_",
            rep(1:4, 2)
          )
        )
      )
    )
  )
})

test_that("aggregate_metad fails for invalid K", {
  expect_error(aggregate_metad(tibble()))
  expect_error(aggregate_metad(tibble(), K = 0))
  expect_error(aggregate_metad(tibble(), K = 1))
})

test_that("fit_metad works", {
  expect_s3_class(fit_metad(N ~ 1, sim_metad(), empty = TRUE), "brmsfit")
})

test_that("fit_metad fails for invalid K", {
  expect_error(fit_metad(N ~ 1, sim_metad(), K = 0, empty = TRUE))
})

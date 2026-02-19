test_that("stancode_metad works", {
  expect_type(stancode_metad(2), "character")
  expect_error(stancode_metad(1))
})

test_that("get_dist works", {
  m <- fit_metad(N ~ 1, sim_metad(), empty = TRUE)
  expect_type(get_dist(m), "closure")
  expect_type(get_dist(m, "lccdf"), "closure")
  expect_error(get_dist(m, "lpdf"))

  brm(response ~ 1, sim_metad(), family = "bernoulli", empty = TRUE) |>
    get_dist() |>
    expect_error()
})

test_that("get_metac works", {
  fit_metad(N ~ 1, sim_metad(), empty = TRUE) |>
    get_metac() |>
    expect_equal("absolute")

  fit_metad(N ~ 1, sim_metad(), empty = TRUE, metac_absolute = FALSE) |>
    get_metac() |>
    expect_equal("relative")

  brm(response ~ 1, sim_metad(), family = "bernoulli", empty = TRUE) |>
    get_metac() |>
    expect_error()
})

test_that("get_stimulus works", {
  fit_metad(N ~ 1, sim_metad(), empty = TRUE) |>
    get_stimulus() |>
    expect_equal("stimulus")

  sim_metad() |>
    rename(manipulation = stimulus) |>
    mutate(joint_response=joint_response(response, confidence, 4)) |>
    fit_metad(joint_response | vint(manipulation) ~ 1, data=_,
      empty = TRUE, categorical = TRUE, .stimulus = "manipulation"
    ) |>
    get_stimulus() |>
    expect_equal("manipulation")

  brm(response ~ 1, sim_metad(), family = "bernoulli", empty = TRUE) |>
    get_stimulus() |>
    expect_error()
})

test_that("normal cumulative distribution functions work", {
  expect_equal(normal_lcdf(0, 0), log(.5))
  expect_equal(normal_lccdf(0, 0), log(.5))
})

test_that("metad_pmf works", {
  metad_pmf(
    stimulus = 0, dprime = 2, c = .5, meta_dprime = 1, meta_c = .5,
    meta_c2_0 = c(0, -.5), meta_c2_1 = c(1, 1.5)
  ) |>
    sum() |>
    expect_equal(1)

  metad_pmf(
    stimulus = 0, dprime = 2, c = .5, meta_dprime = 1, meta_c = .5,
    meta_c2_0 = c(1, -.5), meta_c2_1 = c(1, 1.5)
  ) |>
    expect_error()
})

test_that("metad works", {
  expect_s3_class(metad(3), "brmsfamily")
  expect_equal(metad(2)$name, "metad__2__normal__absolute__multinomial")
  expect_equal(
    metad(2, metac_absolute = FALSE)$name,
    "metad__2__normal__relative__multinomial"
  )
  expect_equal(
    metad(2, distribution = "gumbel_min")$name,
    "metad__2__gumbel_min__absolute__multinomial"
  )
})

## code to prepare data and model for function examples
set.seed(12345)
example_data <- sim_metad(N_trials = 1000)
usethis::use_data(example_data, overwrite = TRUE, compress = "xz")

example_model <- fit_metad(N ~ 1, example_data, iter = 500)
usethis::use_data(example_model, compress = "xz", overwrite = TRUE)

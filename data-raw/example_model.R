## code to prepare `DATASET` dataset goes here

set.seed(12345)
example_data <- sim_metad(N_trials=1000)
example_model <- fit_metad(N ~ 1, example_data, iter=500, chains=1)
usethis::use_data(example_data, example_model,
                  compress = "xz",
                  internal = TRUE, overwrite = TRUE)

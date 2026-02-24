# Sample from a matrix-normal distribution

Sample from a matrix-normal distribution

## Usage

``` r
rmatrixnorm(mu, L_sigma_rows, L_sigma_cols)
```

## Arguments

- mu:

  a matrix of means

- L_sigma_rows:

  the Cholesky-decomposed covariance matrix for the rows

- L_sigma_cols:

  the Cholesky-decomposed covariance matrix for the columns

## Value

A single sample from a matrix-normal distribution with mean `mu` (a
matrix), row-wise covariances `sigma_rows`, and column-wise covariances
`sigma_cols`, where `L_sigma_rows` and `L_sigma_cols` are the
Cholesky-decomposed covariance matrices

## Examples

``` r
mu <- matrix(rep(0, 8), nrow = 4)
sd_rows <- rep(1, 4)
sd_cols <- rep(1, 2)
r_rows <- cor_matrix(.25, 4)
r_cols <- cor_matrix(.75, 2)
L_sigma_rows <- chol(cov_matrix(sd_rows, r_rows))
L_sigma_cols <- chol(cov_matrix(sd_cols, r_cols))
rmatrixnorm(mu, L_sigma_rows, L_sigma_cols)
#>            [,1]        [,2]
#> [1,]  2.9141773 1.694538473
#> [2,] -0.1003886 0.455795483
#> [3,]  0.3722760 0.001755734
#> [4,]  1.0049586 0.025138788
```

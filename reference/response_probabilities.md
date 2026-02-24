# Compute joint response probabilities from aggregated counts

Compute joint response probabilities from aggregated counts

## Usage

``` r
response_probabilities(counts)
```

## Arguments

- counts:

  A vector (or matrix) of counts of joint type 1/type 2 responses as
  provided by
  [aggregate_metad](https://metacoglab.github.io/hmetad/reference/aggregate_metad.md)

## Value

A vector (or matrix) of response probabilities \\P(R, C \\\vert\\ S)\\

## Details

For response \\R\\, confidence \\C\\, stimulus \\S\\, and number of
confidence levels \\K\\, `counts` should be a vector (or matrix with
rows) of the form: \$\$ \[N\_{S=0, R=0, C=K}, \ldots, N\_{S=0, R=0,
C=1}, \\ N\_{S=0, R=1, C=1}, \ldots, N\_{S=0, R=1, C=K}, \\ N\_{S=1,
R=0, C=K}, \ldots, N\_{S=1, R=0, C=1}, \\ N\_{S=1, R=1, C=1}, \ldots,
N\_{S=1, R=1, C=K}\] \\ \$\$

Returns a vector (or matrix with rows) of the form: \$\$ \[P(R=0, C=K
\\\vert\\ S=0), ..., P(R=0, C=1 \\\vert\\ S=0), \\ P(R=1, C=1 \\\vert\\
S=0), ..., P(R=1, C=K \\\vert\\ S=0), \\ P(R=0, C=K \\\vert\\ S=1), ...,
P(R=0, C=1 \\\vert\\ S=1), \\ P(R=1, C=1 \\\vert\\ S=1), ..., P(R=1, C=K
\\\vert\\ S=1)\] \$\$

## Examples

``` r
# Aggregate responses from simulated data
d <- sim_metad() |> aggregate_metad()

# Compute conditional response probabilities
response_probabilities(d$N)
#>      N_0_1 N_0_2 N_0_3 N_0_4 N_0_5 N_0_6 N_0_7 N_0_8 N_1_1 N_1_2 N_1_3 N_1_4
#> [1,]  0.08  0.24  0.12  0.36   0.1  0.08     0  0.02     0  0.04  0.06  0.22
#>      N_1_5 N_1_6 N_1_7 N_1_8
#> [1,]  0.24   0.2  0.08  0.16

# Also works on matrices
matrix(rep(1, 16), nrow = 2) |> response_probabilities()
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,] 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25
#> [2,] 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25
```

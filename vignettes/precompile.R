# Pre-compiled vignettes that depend on API key
# Must manually move image files from hmetad/ to hmetad/vignettes/ after knit
devtools::install()
knitr::knit("vignettes/_categorical.Rmd.src", "vignettes/categorical.Rmd")


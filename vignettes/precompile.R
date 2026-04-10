# Pre-compiled vignettes that require extensive model fitting
# Must manually move image files from hmetad/ to hmetad/vignettes/ after knit
devtools::install()
knitr::knit("vignettes/src/_categorical.Rmd", "vignettes/categorical.Rmd", envir = new.env())
devtools::uninstall()
devtools::load_all()

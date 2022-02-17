# other packages required for gitlab CI
library(DT)
library(covr)
library(devtools)
library(remotes)
install.packages("textshaping", type = "binary")
devtools::install_version("textshaping", version = "0.3.5")
library(textshaping)
library(pkgdown)

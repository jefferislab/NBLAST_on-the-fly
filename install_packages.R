## Basic packages
if(!require("devtools")) install.packages("devtools", dependencies=TRUE)

# have been occasions where this is preferred to CRAN version
if(!require("git2r")) devtools::install_github("ropensci/git2r", dependencies=TRUE)
if(!require("shiny")) install.packages("shiny", dependencies=TRUE)
if(!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
if(!require("downloader")) install.packages("downloader", dependencies=TRUE)

# really just so that we can see the result of installing it directly
# since this is the package that most regularly causes trouble
if(!require("rgl")) install.packages("rgl", dependencies=TRUE)

## nat packages
# we want the github version - nat.flybrains should install this, but let's make certain
devtools::install_github("jefferis/nat", dependencies=TRUE)
# so that we def get the GitHub version not the CRAN version
devtools::install_github("jefferislab/nat.templatebrains", dependencies=TRUE)
devtools::install_github("jefferislab/nat.flybrains", dependencies=TRUE)
# CRAN version might be fine, but let's go with GitHub again
devtools::install_github("jefferislab/nat.nblast", dependencies=TRUE)
devtools::install_github("jefferis/flycircuit", dependencies=TRUE)
devtools::install_github("jefferis/vfbr")

## download our standard set of registrations
library(nat.flybrains)
download_jefferislab_registrations()

## Extras for shiny app
if(!require("shinyURL")) devtools::install_github("aoles/shinyURL", dependencies=TRUE)

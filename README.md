# NBLAST on-the-fly
This project provides a simple interactive demonstration of the [NBLAST](http://flybrain.mrc-lmb.cam.ac.uk/si/nblast) neuron search algorithm using the [RStudio shiny](http://shiny.rstudio.com/) web application framework for R.

## Live demo

A live demo of this application is running at <http://jefferislab.org/si/nblast/on-the-fly/>
## Running locally

### Pre-requsisites

* R > 3.1.1
* RStudio

### Installation 
From an interactive R session:
```r
# several packages are not on CRAN and require devtools for installation
if(!require("devtools")) {
  install.packages("devtools")
  library(devtools)
}

install_github("jefferis/flycircuit", dependencies=TRUE)
install_github("jefferislab/nat.flybrains", dependencies=TRUE)
install.packages(c("shiny","ggplot2"))

install_github("trestletech/shinyRGL",)
install_github("AnalytixWare/ShinySky")
install_github("rstudio/shiny-incubator")
```
### Data
You will also need to download our processed/registered version of the 
[flycircuit.tw](http://flycircuit.tw) dataset. The flycircuit package will 
enable you to do this. Note that the data will be installed in a default location
within the home folder of the current user; therefore you must download the data
## Setting up a server

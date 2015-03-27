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
while running as the appropriate user.

```r
library(flycircuit)

allbyall=load_si_data("allbyallblastcanon_f9dc90ce5b2ffb74af37db1e3a2cb35b.desc")
dps=load_si_data("dpscanon_f9dc90ce5b2ffb74af37db1e3a2cb35b.rds")
apres16k.p0=load_si_data("apres16k.p0.rds")
```

### Running Shiny
```
library(shiny)
runGitHub("jefferislab/NBLAST_on-the-fly")
```
## Setting up a server
If you want to set up a server of your own, perhaps to provide access to a new dataset, besides R and and the packages already mentioned you will need to instally shiny server. There is a free, open source shiny server edition. Detailed instructions for different linux platforms are here:

http://www.rstudio.com/products/shiny/download-server/

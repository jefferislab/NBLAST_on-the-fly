# NBLAST on-the-fly
This project provides a simple interactive demonstration of the [NBLAST](http://flybrain.mrc-lmb.cam.ac.uk/si/nblast) neuron search algorithm using the [RStudio shiny](http://shiny.rstudio.com/) web application framework for R.

## Live demo

A live demo of this application is running at <http://jefferislab.org/si/nblast/on-the-fly/>
## Running locally
You can actually run the nblast server on your own machine with relatively little effort. Instructions follow or watch the [video run-through](https://www.youtube.com/watch?v=cCDql94lSwI).

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
install.packages(c("shiny","ggplot2", "downloader"))
```
### Data
You will also need to download our processed/registered version of the 
[flycircuit.tw](http://flycircuit.tw) dataset. The flycircuit package will 
enable you to do this. Note that the data will be installed in a default location
within the home folder of the current user; therefore you must download the data
while running as the user that will be used to run the shiny app.

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

If you want to hack NBLAST_on-the-fly: 

1. git clone the repository. 
2. Use RStudio to edit the code (recommended)
3. Run the shiny app with runApp

```r
setwd("/path/to/NBLAST_on-the-fly")
library(shiny)
runApp()
```

You will need to run the app in browser such as Chrome that supports WebGL 
rather than in the RStudio browswer if you want to see the 3D content.
## Setting up a server
If you want to set up a server of your own, perhaps to provide access to a new dataset, besides R and and the packages already mentioned you will need to install shiny server. There is a free, open source shiny server edition. Detailed instructions for different linux platforms are here:

http://www.rstudio.com/products/shiny/download-server/

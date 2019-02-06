# download the data required for the app
# note that the data location will de determined by the flycircuit package
# which in turn makes use of the rappdirs packages to choose a system-appropriate
# location in the user's home folder
library(flycircuit)

message("Downloading (updated) data objects to: ", getOption("flycircuit.datadir"))

# this is effectively just a dummy dps object that should trigger download of
# all files
# a slightly different one is requested in server.R
dps<-read.neuronlistfh("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/dpscanon.rds",
											 + 	localdir=getOption('flycircuit.datadir'))
remotesync(dps, download.missing = TRUE)
fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/allbyallblastcanon_f9dc90ce5b2ffb74af37db1e3a2cb35b.desc', type = 'bigmat')
apres16k.p0=load_si_data("apres16k.p0.rds")

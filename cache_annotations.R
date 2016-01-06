# Small script to update the VFB ids and annotation map
# make sure you source this in the NBLAST_on-the-fly project folder by doing e.g.
# 
# source("/path/to/NBLAST_on-the-fly/cache_annotations.R", chdir=TRUE)

library(downloader)

vfb_ids <- read.table("http://www.virtualflybrain.org/public_resources/fc_name_mapping.csv", sep=",", header=TRUE)
saveRDS(vfb_ids, file="vfb_ids.rds")

download("https://raw.githubusercontent.com/VirtualFlyBrain/VFB_owl/master/doc/annotation_map.tsv", "annotation_map.tsv")
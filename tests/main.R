# rempédzs

# Needed
## install.packages(c(NULL
## 	, "ncdf4"
## 	, "rgdal"
## 	, "divDyn"
## 	, "chronosphere"
## 	, "tinytest"
## 	, "terra"
## 	, "via"
##  , "httr2"
##  , "geojsonsf"
##  , "sf"
## ))


library(tinytest)
library(parallel)

# enforce correct names
library(rampage)
library(rgplates)

if(rgplates:::getOS()=="linux") wd <- file.path(Sys.getenv("Dropbox"), "Software/rampage")
if(rgplates:::getOS()=="windows") wd <- file.path("D:/rampage")
if(rgplates:::getOS()=="osx") wd <- file.path("~/Desktop/rampage")

setwd(wd)

# make a cluster of 8
cl <- parallel::makeCluster(4, outfile="")
parallel::clusterCall(cl, source, "rampage/tests/source.R")

# the online
expansion <- run_test_dir("rampage/tests/test_expand")
coloredPoints <- run_test_dir("rampage/tests/test_colorpoints")
legend <- run_test_dir("rampage/tests/test_ramplegend")

# Finish
stopCluster(cl)

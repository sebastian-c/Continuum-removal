#####Parameter setup#####
#INSERT PARAMETERS YOU WISH TO SET HERE

#FOR ALL DIRECTORIES, USE '/' instead of '\'

#Set working directory
wd <- "C:/Research/Fan Deng" #Set this to spectra location

#Set destination directory
dest_wd <- wd  #replace right side of arrow with proposed destination in quotes. 
               #defaults to spectra location

#Set spectra filename
specname <- "for FanSpectra.txt"

#Set boundaries
max_wav <- 350:2500 #Set minimum and maximum for your spectra
int_wav <- 350:2500 #Set the wavelengths in which you are interested

setwd("C:/Research/Fan Deng")

source("Convex hull.r")

fan_spectra <- read.table("for FanSpectra.txt", header=T)

hull_list <- list()

for(i in 1:nrow(fan_spectra)){
  hull_list[[i]] <- c_hull_deviation(fan_spectra[i,], .interval = 419:2500, .all = 419:2500, .return_hull = T)
}

#slightly slower implementation, but with progress bar
library(plyr)

fast_hull_list <- alply(fan_spectra, 1, c_hull_deviation, .interval = 419:2500, .all = 419:2500, .return_hull = T)#, .progress="text")

##loop:
# user  system elapsed 
# 36.11    0.01   36.24 

##plyr function without progress bar
# user  system elapsed 
# 35.69    0.00   35.82
##with progress bar
# 36.55    0.03   36.53
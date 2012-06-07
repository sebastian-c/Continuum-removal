####Instructions####
#Define parameters in the 'Parameter setup' section
#
#####Parameter setup#####
#INSERT PARAMETERS YOU WISH TO SET HERE
#FOR ALL DIRECTORIES, USE '/' instead of '\'

#Set working directory and destination directory
wd <- "C:/Research/Fan Deng" #Set this to spectra location

dest_wd <- wd  #replace right side of arrow with proposed destination in quotes. 
               #defaults to spectra location

#Set input and output spectra filename
specname <- "for FanSpectra.txt"
outname <- "hull-removed spectra.txt"

#Set boundaries
max_wav <- 419:2500 #Set minimum and maximum for your spectra
int_wav <- 419:2500 #Set the wavelengths in which you are interested

#Do you want graphs?
wantgraphs <- FALSE

#MAGIC HAPPENS HERE:

####Actual code####

setwd(wd)
library(plyr)

#Define function determining spectra in interval
in_interval <- function(.all,.interval,...){
  ## index for an interval
  ## a function to subset a particular waveband interval
  .in_interval = .all %in% .interval
}

#Define convex hull function
c_hull_deviation <- function(.spectra, .all = 350:2500, .interval = 350:2500, .return_hull = F,...){
  ## a function to perform deviations from the convex hull
  ## at the moment only tested on reflectance values
  ## not absorbance
  ##
  ## organize data
  ##
  ## get the interval
  .in_interval <- in_interval(.all = .all, .interval = .interval)
  ##
  ## sort the data
  .data <- sortedXyData(.all[.in_interval], .spectra[.in_interval])
  ##
  ## calculate convex hull
  c_hull <- chull(.data)
  ##
  ## get the approprite region
  c_hull <- c_hull[which(c_hull == 1):length(c_hull)]
  ##
  ## calculate linear approximation between hull points
  linear_approx <- approx(.data[c_hull,], xout = .interval, method = 'linear', ties = 'mean')
  ##
  ## calculate the deviation from the convex hull
  deviation <- ( linear_approx[[2]] - .spectra[.in_interval] )/linear_approx[[2]]
  ##
  ## add the hull if you wish
  if(.return_hull == T){attr(deviation, 'hull') <-linear_approx[[2]]}
  ##
  ## return
  return(deviation)
  ##
}

#Define graphing function
spec_graphs <- function(.specvec, .hullvec){
  
}

fan_spectra <- read.table(specname, header=T)
fan_matrix <- as.matrix(fan_spectra)
dimnames(fan_matrix)[[2]]<-substring(names(fan_spectra),2)

fan_matrix_chull<- aaply(fan_matrix, 1,c_hull_deviation, .all=max_wav, .interval=int_wav, .return_hull=FALSE,
                         .progress=progress_win(title="Subtracting convex hull..."))

dest_dir <- file.path(dest_wd, outname)

write.table(fan_matrix_chull, dest_dir)

if(wantgraphs) 
###


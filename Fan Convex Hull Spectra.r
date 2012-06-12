####Instructions####
#Define parameters in the 'Parameter setup' section
#This script returns the spectra corrected for the convex hull

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
library(reshape)
library(tcltk)

#Define function determining spectra in interval
in_interval <- function(.all,.interval,...){
  ## index for an interval
  ## a function to subset a particular waveband interval
  .in_interval = .all %in% .interval
}

#Define convex hull function
c_hull_correction <- function(.spectra, .all = 350:2500, .interval = 350:2500, .return_hull = F,...){
  ## a function to perform correct for convex hull
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
  ## get the appropriate region
  c_hull <- c_hull[which(c_hull == 1):length(c_hull)]
  ##
  ## calculate linear approximation between hull points
  linear_approx <- approx(.data[c_hull,], xout = .interval, method = 'linear', ties = 'mean')
  ##
  ## calculate the corrected spectra
  hull_correct <- .spectra[.in_interval]/linear_approx[[2]]
  ##
  ## add the hull if you wish
  if(.return_hull == TRUE){attr(hull_correct, 'hull') <-linear_approx[[2]]}
  ##
  ## return
  return(hull_correct)
  ##
}

fan_spectra <- read.table(specname, header=T)
fan_matrix <- as.matrix(fan_spectra)
dimnames(fan_matrix)[[2]]<-substring(names(fan_spectra),2)

fan_matrix_chull<- aaply(fan_matrix, 1,c_hull_correction, .all=max_wav, .interval=int_wav, .return_hull=FALSE,
                         .progress=progress_win(title="Subtracting convex hull..."))

dest_dir <- file.path(dest_wd, outname)

write.table(fan_matrix_chull, dest_dir)

if(wantgraphs){
  
  plot_dir <- file.path(dest_wd, "plots")
  dir.create(plot_dir)
  
  #Define graphing function
  spec_graphs <- function(x){
    .specname <- paste(unique(x$spectra))
    location <- file.path(plot_dir, paste(.specname, " hull", ".png", sep=""))
    
    png(file=location)
    par(mfrow=c(1,2))
    plot(x$wavelength,x$"spectrum value",
         xlab="Wavelengths",
         ylab="Reflectance",
         main=paste(.specname, "without continuum removal"),
         type="l")
    plot(x$wavelength,x$"hull corrected value",
         xlab="Wavelengths",
         ylab="Reflectance",
         main=paste(.specname, "hull corrected"),
         type="l",
         col="red")
    
    dev.off()
  }

  spec_melt <- melt(fan_matrix)
  hull_melt <- melt(fan_matrix_chull)
  names(spec_melt) <- c("spectra", "wavelength", "spectrum value")
  names(hull_melt) <- c("spectra", "wavelength", "hull corrected value")
  
  graph_data <- join(spec_melt, hull_melt, by=c("spectra", "wavelength"))
  
  d_ply(graph_data, "spectra", spec_graphs, .progress=progress_win(title="Plotting..."))
  
}

if(Sys.info()['sysname']=="Windows")
  winDialog(type = "ok", "The script completed successfully.") else {
    library(tcltk)
    tk_messageBox(type = "ok", "The script completed successfully.", caption = "Script complete")
    }

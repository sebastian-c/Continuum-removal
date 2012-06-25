#This script is not required, it was a demonstration of the differences between
#deviation and actual convex hull. As the chull function has changed, it is now
#defunct.
source("Fan Convex Hull Spectra.r")

png("Original spectra.png")
plot(colnames(fan_matrix),fan_matrix[37,], type="l", 
     main="Original spectra", xlab="Wavelength", ylab="Absorbance",
     ylim=c(0,1))
dev.off()

png("Deviation spectra.png")
plot(colnames(fan_matrix_chull),fan_matrix_chull[37,], type="l", 
     main="Deviation spectra", xlab="Wavelength", ylab="Absorbance",
     ylim=c(0,1))
dev.off()

png("Deviation superimposed.png")
plot(colnames(fan_matrix),fan_matrix[37,], type="l", 
     main="Spectra with deviation superimposed", xlab="Wavelength", ylab="Absorbance", 
     ylim=c(0,1))
lines(colnames(fan_matrix_chull),fan_matrix_chull[37,], col="red")
dev.off()

#Function to give convex-corrected spectra
c_hull_other <- function(.spectra, .all = 350:2500, .interval = 350:2500, .return_hull = F,...){
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
  ## get the appropriate region
  c_hull <- c_hull[which(c_hull == 1):length(c_hull)]
  ##
  ## calculate linear approximation between hull points
  linear_approx <- approx(.data[c_hull,], xout = .interval, method = 'linear', ties = 'mean')
  ##
  ## calculate the deviation from the convex hull
  deviation <- ( .spectra[.in_interval] )/linear_approx[[2]]
  ##
  ## add the hull if you wish
  if(.return_hull == T){attr(deviation, 'hull') <-linear_approx[[2]]}
  ##
  ## return
  return(deviation)
  ##
}

fan_matrix_chull<- aaply(fan_matrix, 1, c_hull_other, .all=max_wav, .interval=int_wav, .return_hull=FALSE,
                         .progress=progress_win(title="Subtracting convex hull..."))

png("Hull-removed spectra.png")
plot(colnames(fan_matrix_chull),fan_matrix_chull[37,], type="l", 
     main="Hull-removed spectra", xlab="Wavelength", ylab="Absorbance",
     ylim=c(0,1))
dev.off()

png("Hull-removed superimposed.png")
plot(colnames(fan_matrix),fan_matrix[37,], type="l", 
     main="Spectra with hull-removed superimposed", xlab="Wavelength", ylab="Absorbance", 
     ylim=c(0,1))
lines(colnames(fan_matrix_chull),fan_matrix_chull[37,], col="red")
dev.off()

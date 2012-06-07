###REQUIRED###

in_interval <- function(.all,.interval,...){
  ## index for an interval
  ## a function to subset a particular waveband interval
  .in_interval = .all %in% .interval
}
###END REQUIRED###

## ########################################################
## some functions for filtering using 
## deviations from the convex hull 
## also known as continuum removal 
## ########################################################
## 
## depends on
## chunk reference functions-basic
## chunk reference functions-mineralogy
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


# test<-read.csv("C:/Research/Spectra/testspec.csv")
# 
# hull_list <- list()
# 
# for(i in 1:nrow(test)){
#   hull_list[[i]] <- c_hull_deviation(test[i,], .interval = 350:2500, .all = 350:2500, .return_hull = T)  
# }
# 


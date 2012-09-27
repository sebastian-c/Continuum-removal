###REQUIRED###

in_interval <- function(.all,.interval,...){
  ## index for an interval
  ## a function to subset a particular waveband interval
  .in_interval = .all %in% .interval
}

c_hull_deviation <- function(.spectra, .all = 350:2500, .interval = .all, .return_hull = F,...){
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
  deviation <- ( linear_approx[[2]] - .spectra[.in_interval] )/linear_approx[[2]]
  ##
  ## add the hull if you wish
  if(.return_hull == T){attr(deviation, 'hull') <-linear_approx[[2]]}
  ##
  ## return
  return(deviation)
  ##
}
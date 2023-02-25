

#Tempertature Interpolation

sine.approx2 <- function(tmin, tmax, yl = 48) {
  if(!length(tmin) == length(tmax)) stop("length tmin must equal length tmax")
  # tmin = df$TMIN[1:2];tmax = df$TMAX[1:2]; 
  b <- (2*pi)/24  # period = 24 hours
  phi <- pi/2 # horizontal shift
  xout1 <- seq(0,12,length.out = yl)                        #temp falling
  xout2 <- seq(12,24,length.out = yl)[-1]                        #temp falling
  minmax <- cbind(tmin, tmax)
  maxmin <- cbind(tmax[-length(tmax)],tmin[-1])
  ydayL <- list()
  xdayL <- list()
  day <- 0
  
  for(i in 2:length(tmin)){
    up <- c(tmin[i-1], tmax[i-1])
    down <- c(tmax[i-1], tmin[i])
    xup <- xout1 + day * 24
    xdown <- xout2 + day * 24
    yhatup <- diff(up)/2 * sin(b*xup-phi) + mean(up) #up
    yhatdown <- diff(down)/2 * sin(b*xdown+phi) + mean(down) #down
    xdayL[[i-1]] <- c(xup, xdown)
    ydayL[[i-1]] <- c(yhatup, yhatdown)
    day <- 1+day
  }
  # last up addtion
    up <- c(tmin[i], tmax[i])
    xup <- xout1 + day * 24
    yhatup <- diff(up)/2 * sin(b*xup-phi) + mean(up) #up
    xdayL[[i]] <- c(xup)
    ydayL[[i]] <- c(yhatup)

    return(list(x = unlist(xdayL), y = unlist(ydayL)))
}





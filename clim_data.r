getwd()
setwd("C:/Users/jmott1/Documents/Climate Effects on Wheat")
options(max.print=1000)
options(width= 150)

# Read in weather location files/select columns

#install.packages("data.table")
#install.packages("hydroTSM")
library(hydroTSM) # for date conversion to season
library(data.table) # for data import


library(httpgd)

clim_files <- list.files("C:/Users/jmott1/Documents/Climate Effects on Wheat/climate_data/")
clim_files

# "DATE", "NAME", "PRCP", "TMAX", "TMIN"
for(i in 1:length(clim_files)) {
    assign(paste0("data", i),
        fread(paste0("C:/Users/jmott1/Documents/Climate Effects on Wheat/climate_data/", clim_files[i]), 
        select = c("DATE", "STATION", "PRCP", "TMAX", "TMIN"), sep= ",", header = TRUE))
        
}



#temps and PCRP divied by 10 ?? transform function?

data1[,3] <- data1[,3]/10
data1[,4] <- data1[,4]/10
data1[,5] <- data1[,5]/10

data3[,3] <- data3[,3]/10
data3[,4] <- data3[,4]/10
data3[,5] <- data3[,5]/10

data7[,3] <- data7[,3]/10
data7[,4] <- data7[,4]/10
data7[,5] <- data7[,5]/10

#combining datasets (start here to reset data.frame)

clim <- rbind(data1, data2, data3, data4, data5, data6, data7)

#add loc name and number to station (not efficient but it looks cool to me and it works).
clim$loc <- ifelse(clim$STATION == 'USC00440766', "Blacksburg",
                ifelse(clim$STATION == 'USC00441322', "Blackstone",
                    ifelse(clim$STATION == 'USC00444044', "Holland",
                        ifelse(clim$STATION == 'USC00446712', "Orange",
                            ifelse(clim$STATION == 'USC00446475', "Painter",
                                ifelse(clim$STATION == 'USC00449263', "Shenandoah", "Warsaw"))))))

clim$locnum <- ifelse(clim$STATION == 'USC00440766', 1,
                ifelse(clim$STATION == 'USC00441322', 2,
                    ifelse(clim$STATION == 'USC00444044', 3,
                        ifelse(clim$STATION == 'USC00446712', 4,
                            ifelse(clim$STATION == 'USC00446475', 5,
                                ifelse(clim$STATION == 'USC00449263', 6, 7))))))
head(clim)
tail(clim)

#date to year, month, day in columns

clim$DATE <- as.Date(clim$DATE, format = "%m/%d/%Y")

clim$year <- as.numeric(format(as.Date(clim$DATE, format = "%m/%d/%Y"), "%Y"))
clim$month <- as.numeric(format(as.Date(clim$DATE, format = "%m/%d/%Y"), "%m"))
clim$day <- as.numeric(format(as.Date(clim$DATE, format = "%m/%d/%Y"), "%d"))

str(clim)


# season column addition and set to ~ equinox/solstice date 

clim$season <- ifelse((clim$month == 6 & clim$day <=21), "spring",
                    ifelse((clim$month == 3 & clim$day <=21), "winter",
                    ifelse((clim$month == 12 & clim$day <=21), "autumm", 
                    ifelse((clim$month == 9 & clim$day <=21), "summer",
                                        time2season(clim$DATE, out.fmt = "seasons")))))


clim[month == 9 & day <= 22] # test equinox/soltice months



#Traditional Growing Degree Days Calculation for wheat
#gdd = (tmax + tmin)/2

clim$gdd <- (clim$TMAX + clim$TMIN)/2

head(clim)
tail(clim)


#Interpolated Degree Days

# subset a month for testing (April 2015 in Blackstone)

df <- clim[month == 2 & year == 2015 & locnum == 2]


#prepare max and min matrix

days <- length(df$day)
tmax <- matrix(df$TMAX)
tmin <- matrix(df$TMIN)

m <- matrix(NA, ncol=days*2, nrow=1)
m[,seq(1,ncol(m),2)] <- tmin
m[,seq(2,ncol(m)+1,2)] <- tmax



# {\displaystyle y(t)=A\sin(2\pi ft+\varphi )=A\sin(\omega t+\varphi )}


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

#sine interpolation output
ysine <- sine.approx2(tmin = matrix(df$TMIN), tmax = matrix(df$TMAX)) 

#Sample Plot

grid <- matrix(c(1,2), nrow=1, ncol=2)

layout(grid, width = c(3,1))

plot(ysine$x/24, ysine$y, pch = 16, cex = 0.3, xlab = "Day", ylab = "Temperature (°C)")
points(seq(0, 27,  by=1), matrix(df$TMIN), pch = 16, col = "blue", cex = 1.5)
points(seq(0.5, 27.5,  by=1), matrix(df$TMAX), pch = 16, col = "red", cex = 1.5)
abline(h= c(seq.int(-20, 40, 5)), col= "grey", lty = 2)

#convert exposure bin times to GDD 


exp <- exposurebins(dailytemp= ysine$y)

#monthly exposure totals
mtotalexp <- apply(exp, 1, function(x){
      ((x*15)/60)/24
   }
)

barplot(mtotalexp, horiz=TRUE, xlab = "Time at Temp Exposure (days)", width = 5, space = 0)

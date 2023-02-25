#full dataset analysis

clim <- clim[year >= 1990 & year <= 2021]

clim_list <- split(clim, list(clim$month, clim$year, clim$locnum), drop = TRUE) #will drop empty months 



clim_list[['3.2005.2']]


climexp <- list()


system.time(

for (i in 1:length(clim_list)){
    #i='3.2005.2' #month, year, loc
    mdt <- clim_list[[i]]
    
    mdt$TMAX[is.na(mdt$TMAX)] <- mean(mdt$TMAX, na.rm = TRUE)
    mdt$TMIN[is.na(mdt$TMIN)] <- mean(mdt$TMIN, na.rm = TRUE)
    
    tmx <- as.matrix(mdt[,4])
    tmn <- as.matrix(mdt[,5])
 
    
    
    #sine interpolation
    ysine <- sine.approx2(tmin = tmn, tmax = tmx) 
    temps <- ysine$y
    
    
    
    daydf <- apply(mdt, 2, function(x){
        replicate(96, x)})

        daydf <- as.data.frame(daydf)
        daydf$day <- as.numeric(daydf$day)

        daydf <- daydf[sort(daydf$day),]

        daydf <- head(daydf, -(length(daydf$day) - length(ysine$y)))

        qtemp <- cbind(daydf, temps) #merge temp interpolation with daily times

        qlist <- split(qtemp, qtemp$day)

        #exposure to bins 

        bins <- matrix(NA, nrow = length(mdt$day), ncol = 16)
            
            for (j in 1:length(qlist)){
                #j=18
                g <- qlist[[j]][,13]
                daybin <- table(cut(g, breaks = seq.int(-35,45,5))) #bin sizes 
                bins[j, ] <- as.numeric(daybin)
                }
  
    colnames(bins) <- names(daybin)
    # rownames(bins) <- seq.int(1, length(qlist), 1)
    
    mdtexp <- cbind(mdt,bins)

    climexp[[i]] <- mdtexp
   
    
   
}

)

expdata <- as.data.frame(do.call(rbind, climexp))

copy1 <- expdata

# add harvest year

copy1$harvyr <- ifelse(expdata$month == 9 | expdata$month == 10 | expdata$month == 11 | expdata$month == 12, expdata$year + 1, expdata$year)
copy1$TAVG <- (copy1$TMAX+copy1$TMIN)/2
unique(copy1$harvyr)


datetest <- copy1

# match dates with plant/harv by location and year


fulldate <- read.csv("C:/Users/jmott1/Documents/Climate Effects on Wheat/LocPlantHarvDate.csv", sep = ",", header = TRUE)

fulldate$Planting.Date <- as.Date(fulldate$Planting.Date, "%m/%d/%Y")
fulldate$Harvest.Date <- as.Date(fulldate$Harvest.Date, "%m/%d/%Y")


str(fulldate)
tail(fulldate)

locdate <- list()


for (i in 1:length(fulldate$loc)){
    #i = 1
    loc <- fulldate$loc[[i]]
    locnum <- fulldate$locnum[[i]]
    
    a <- fulldate$Planting.Date[[i]]
    b <- fulldate$Harvest.Date[[i]]
    dates <- c(seq(a, b, by = "days"))
    
    x <- list()
        x$DATE <- paste0(as.Date(dates))
        x$loc <- rep(loc, length(dates))
        x$locnum <- rep(locnum, length(dates))

    locdate[[i]] <- do.call(cbind, x)

}


locplantdate <- as.data.frame(do.call(rbind, locdate))
locplantdate$DATE <- as.Date(locplantdate$DATE)



# remove days with no wheat by location

obsclim <- merge(locplantdate, copy1, by.x = c("DATE", "locnum"), by.y = c("DATE", "locnum")) 
obsclim$season <- as.factor(obsclim$season)
obsclim$locnum <- as.numeric(obsclim$locnum)
str(obsclim)

#compute summaries for gdd bins by location, year (and season). 


obsclim_list <- split(obsclim, list(obsclim$locnum, obsclim$harvyr, obsclim$season), drop = TRUE) 

binCols <- grep("^\\(.*\\)$|^\\[.*\\)$|^\\(.*\\]$|^\\[.*\\]$", names(obsclim))

locBinL <- list()
for(i in unique(obsclim$locnum)){
    # i = 4
    obsclimi <- obsclim[obsclim$locnum == i,]
    for(j in unique(obsclim$harvyr)){
        # j = 1999
        obsclimij <- obsclimi[obsclimi$harvyr == j,]
        
            sL <- list()
            for(k in unique(obsclimi$season)){
            # k = "autumm"
                obsclimijk <- obsclimij[obsclimij$season == k, ]
                binij <- colSums(obsclimij[binCols])
                names(binij) <- paste0(k, "_", names(binij))        
                sL[[k]] <- binij
            }
            locBinL[[paste0("loc", i)]][[j]] <- Reduce(c, sL)            

    }
    locBinL[[paste0("loc", i)]] <- data.frame(harvyr = unique(obsclim$harvyr), do.call(rbind, locBinL[[paste0("loc", i)]]), check.names = FALSE)
}

binSummary <-  data.frame(locnum = unique(obsclim$locnum), do.call(rbind, locBinL), check.names = FALSE)

write.csv(binSummary, "C:/Users/jmott1/Documents/Climate Effects on Wheat/binSummary.csv", row.names=FALSE) 
# write.csv(copy1, "C:/Users/jmott1/Documents/Climate Effects on Wheat/expdata.csv", row.names=FALSE)



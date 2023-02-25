

# seasonal average temperature

dt1 <- split(copy1, list(copy1$season))


figgrid <- matrix(1:4, ncol=2)


layout(figgrid, width = c(1,1), height = c(1,1))

boxplot(TAVG ~ year, data = dt1$autumm, main = "AUTUMN", xlab = "year", ylab = "Average Temp (C)")
boxplot(TAVG ~ year, data = dt1$winter, main = "WINTER", xlab = "year", ylab = "Average Temp (C)")
boxplot(TAVG ~ year, data = dt1$spring, main = "SPRING", xlab = "year", ylab = "Average Temp (C)")
boxplot(TAVG ~ year, data = dt1$summer, main = "SUMMER", xlab = "year", ylab = "Average Temp (C)")
mtext("Seasonal Temp Averages", side = 3, line = -2, outer = TRUE)


# seasonal average MAX temp

dt2 <- split(copy1, list(copy1$season))


layout(figgrid, width = c(1,1), height = c(1,1))

boxplot(TMAX ~ year, data = dt2$autumm, main = "AUTUMN", xlab = "year", ylab = "Average Max Temp (C)")
boxplot(TMAX ~ year, data = dt2$winter, main = "WINTER", xlab = "year", ylab = "Average Max Temp (C)")
boxplot(TMAX ~ year, data = dt2$spring, main = "SPRING", xlab = "year", ylab = "Average Max Temp (C)")
boxplot(TMAX ~ year, data = dt2$summer, main = "SUMMER", xlab = "year", ylab = "Average Max Temp (C)")
mtext("Seasonal MAX Temp Averages", side = 3, line = -2, outer = TRUE)


# seasonal average MIN temp

dt3 <- split(copy1, list(copy1$season))


layout(figgrid, width = c(1,1), height = c(1,1))

boxplot(TMIN ~ year, data = dt3$autumm, main = "AUTUMN", xlab = "year", ylab = "Average Min Temp (C)")
boxplot(TMIN ~ year, data = dt3$winter, main = "WINTER", xlab = "year", ylab = "Average Min Temp (C)")
boxplot(TMIN ~ year, data = dt3$spring, main = "SPRING", xlab = "year", ylab = "Average Min Temp (C)")
boxplot(TMIN ~ year, data = dt3$summer, main = "SUMMER", xlab = "year", ylab = "Average Min Temp (C)")
mtext("Seasonal MIN Temp Averages", side = 3, line = -2, outer = TRUE)




# precipitation cumulative

layout(figgrid, width = c(1,1), height = c(1,1))

boxplot(PRCP ~ year, data = dt1$autumm, main = "AUTUMN", xlab = "year", ylab = "Cumulative Precip (cm)")
boxplot(PRCP ~ year, data = dt1$winter, main = "WINTER", xlab = "year", ylab = "Cumulative Precip (cm)")
boxplot(PRCP ~ year, data = dt1$spring, main = "SPRING", xlab = "year", ylab = "Cumulative Precip (cm)")
boxplot(PRCP ~ year, data = dt1$summer, main = "SUMMER", xlab = "year", ylab = "Cumulative Precip (cm)")
mtext("Seasonal total precip", side = 3, line = -2, outer = TRUE)






library(spatstat)
data(finpines)
finpines 

clm<-as.data.frame(finpines)

variogram.cloud <- data.frame(distance=NA, gamma=NA)
variogram.names <- c("distance", "gamma")
colnames(variogram.cloud) <- variogram.names

for (i in 1:125) {
  for (j in (i+1):126) {
    d <- sqrt(sum((clm[i, c("x", "y")] - clm[j, c("x","y")])^2))
    gamma <- (clm[i, "height"] - clm[j, "height"])^2 / 2
    variogram.cloud <- rbind(variogram.cloud, c(d, gamma))
  }
}
  
plot(variogram.cloud, xlab="Distance", ylab="$\\gamma$", cex=0.1)

#this looks like there are white lines because the data was rounded to the whole numbers hence
#not printing 0.5 and 0.4 and so on 

#now separating 0 to 6 into bins 
start <- 0
end <- 9
number.of.bins <- 60
bins <- seq(start, end, length.out=number.of.bins)
bin.width <- bins[2] - bins[1]
bin.midpoints <- head(bins, -1) + (bin.width/2)

variogram.cloud$bin <- cut(variogram.cloud$distance, bins)
experimental.variogram <- tapply(variogram.cloud$gamma, variogram.cloud$bin, mean)
plot(bin.midpoints, experimental.variogram, xlab="Distance")

exponential.variogram <- lm(experimental.variogram ~ exp(bin.midpoints))
lines(bin.midpoints, predict(exponential.variogram), col=2)
#red colour is the exponential 

cubic.variogram <- lm(experimental.variogram ~ I(bin.midpoints ^ 3))
lines(bin.midpoints, predict(cubic.variogram), col=3)
#green one is cubic function 


spherical.variogram <- lm(spherical.variogram ~)
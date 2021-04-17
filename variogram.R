library(spatstat)
library(gstat)
data(bronzefilter)

bronzefilter<-as.data.frame(bronzefilter) #bronzefilter from spatstat

v <- variogram(marks ~ 1, ~x + y, bronzefilter, cutoff=10, width=0.3)
v.spherical <- fit.variogram(v, vgm("Sph"))
v.exponential <- fit.variogram(v, vgm("Exp"))
v.gaussian <- fit.variogram(v, vgm("Gau"))


par(mfrow=c(2,2))
plot(v, v.spherical)
plot(v, v.exponential)
plot(v, v.gaussian)

# make cloud
v <- variogram(marks ~ 1, ~x + y, bronzefilter, cutoff=10, width=0.3, cloud=TRUE)
plot(v)

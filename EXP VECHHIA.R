#analysis of MODIS temperature data from Heaton et al (2018)
 
library("fields")
library("GpGp")

load("modis_temps.RData")
modis_temps[1:30,]
nrow(modis_temps)

temp_dims <- c(500,300)
temp_array <- array(modis_temps$temp, temp_dims)
image.plot(temp_array)

# number of observations
n.obs <- sum(!is.na(modis_temps$temp))
keep.sample <- runif(nrow(modis_temps)) < 0.7
modis_temps$subtemp <- rep(NA, nrow(modis_temps))
modis_temps$subtemp[keep.sample] <- modis_temps$temp[keep.sample]
# number of observations in subtemp2:
n.subtemp.obs <- sum(!is.na(modis_temps$subtemp))

#subsampled data (to make things run faster)
image.plot(array(modis_temps$subtemp, temp_dims))

#detect missing values
not_missing <- !is.na(modis_temps$subtemp)

#how many datapoints are not missing
#i.e how many points we have in out subdata
sum(not_missing)
#response 
y <- modis_temps$subtemp[not_missing]


#sample() takes random vaues 
?sample()
test_indicies <- sample(1:n.subtemp.obs, 1000)
#this randomly takes 1000 values from a set of 1 to 80 000


#locations (log and lat)
locs <- cbind( modis_temps$lon[not_missing], modis_temps$lat[not_missing])

#linear covariates 
X <- cbind( rep(1,sum(not_missing)), locs)

#creating the Test set whih we will use to check the accuracy
#1000 locations from training data

TestSetX <- X[test_indicies,] 

#same thing byt for y values 

TestSetY <- y[test_indicies]


# -test_indicies, it gives all the rows that are not in the
# test_indicies, therefore to clear our data from test set
#we just use -test_indicies

#Hence, the data we use for simulation now is

y <- y[-test_indicies]
X <- X[-test_indicies,]

#and so same for locs 

locs <- locs[-test_indicies,]


#fit an exponential covarinace GP model using GpGp::fir_model
?fit_model
t1<- proc.time()
gpfit <- fit_model( y = y,locs = locs, X =X , "exponential_sphere")
print(proc.time()-t1)
#exponential covariance = theta1*(exp(-dist/theta2)+theta3*(dist==0))


#print out summary 
summary(gpfit)

earth_radius <- 6356
gpfit$covparms[2]*earth_radius

#set up prediction location and design matrix 
pred_inds <- is.na(modis_temps$subtemp)
locs_pred <- as.matrix(modis_temps[pred_inds, c("lon","lat")])
X_pred <- cbind(rep(1,sum(pred_inds)),locs_pred)

#do prediction of the missing data
?predictions
t1 <- proc.time()
pred <- predictions(fit= gpfit, locs_pred=locs_pred,X_pred=X_pred,m=30)
print(proc.time()-t1)

#make a map of the predictions 
obs_and_pred <- modis_temps$subtemp
obs_and_pred[pred_inds]<- pred

par(mfrow=c(1,2))
image.plot( array(modis_temps$subtemp, temp_dims), axes = FALSE)
mtext("Data")
image.plot(array(obs_and_pred, temp_dims), axes=FALSE)
mtext("Conditional Expectation (Prediction)")

#do conditional simulations 
#SIMULATE missing data, but consistent with the observed data
ncondsim <- 30
t1 <- proc.time()
sims <- cond_sim(fit=gpfit, locs_pred=locs_pred,X_pred = X_pred,
                 nsims = ncondsim, m=30)
print(proc.time()-t1)

#plot 5 out of 30 conditional simulations 
zlims <- range( c(modis_temps$subtemp, sims[,1:5]), na.rm = TRUE )
for (j in 1:5){
  obs_and_sim <- modis_temps$subtemp
  obs_and_sim[pred_inds] <- sims[,j]
  par(mfrow=c(1,2))
  image.plot( array(obs_and_sim,temp_dims), axes = FALSE, zlim=zlims)
  mtext(paste("Conditional Simulation",j))
}

#prediction standard deviations 
sum_squares <- rep(0, 500*300)
for(j in 1:30){
  obs_and_sim <- modis_temps$subtemp
  obs_and_sim[pred_inds] <- sims[,j]
  sum_squares <- sum_squares + (obs_and_pred - obs_and_sim)^2
}
pred_rmse <- sqrt(1/30*sum_squares)
#plot of data, predictions, 1 conditional simulation,prediction sandard devs
par(mfrow=c(2,2), mar=c(1,1,3,3))
image.plot( array(modis_temps$subtemp, temp_dims), axes=FALSE)
mtext("Data")
image.plot( array(obs_and_pred, temp_dims), axes=FALSE)
mtext("Conditional Expectation (Prediction)")
image.plot( array(obs_and_sim, temp_dims), axes=FALSE)
mtext("One Conditional Simulation")
image.plot( array(pred_rmse, temp_dims), axes=FALSE)
mtext("Prediction Standard Deviation")



#so this approach made it possible to predict temperature
#for the points representing white areas, therefore
#we can conclude that Vacchila approxiamtion
#allows us to condition on a lagre dataset(80K) (which is out subdata)
#whereas with GP we only can condition on up to (50K) 
#locations 

#checking on the accuracy of the model 

























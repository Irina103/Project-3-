library(spNNGP)
library(ggplot2)
library(fields)

load("modis_temps.RData")

temp_dims <- c(500, 300)
temp_array <- array(modis_temps$temp, temp_dims)
image.plot(temp_array)
image.plot(array(modis_temps$subtemp, temp_dims))

not_missing <- !is.na(modis_temps$subtemp)

# define the response
y <- modis_temps$subtemp[not_missing]

# locations
locs <- cbind(modis_temps$lon[not_missing], modis_temps$lat[not_missing])

##Fit a NNGP model
n.samples <- 100  # Note this is number of MCMC samples, not observations.
starting <- list("phi"=6, "sigma.sq"=5, "tau.sq"=1)
tuning <- list("phi"=0.5, "sigma.sq"=0.5, "tau.sq"=0.5)
priors <- list("phi.Unif"=c(3/1, 3/0.01), "sigma.sq.IG"=c(2, 5), "tau.sq.IG"=c(2, 1))
cov.model <- "exponential"
n.report <- 10
n.omp.threads <- 4 # !!! this will need to be changed depending on the computer it's running on (4 is probably OK)

nngp.model <- spNNGP(y~1, coords=locs, starting=starting, method="response", n.neighbors=30,
              tuning=tuning, priors=priors, cov.model=cov.model,
              n.samples=n.samples, n.omp.threads=n.omp.threads, n.report=n.report)

# Make predictions on the unobserved locations using the model
# set up locations for predictions
pred_inds <- is.na(modis_temps$subtemp) # making predictions for everywhere where it's white
locs_pred <- as.matrix(modis_temps[pred_inds, c("lon", "lat")]) # longitudes and latitudes for missing data

nngp.preds <- predict(nngp.model, X.0 = as.matrix(rep(1, nrow(locs_pred))), coords.0 = locs_pred, n.omp.threads=n.omp.threads)
pred <- apply(nngp.preds$p.y.0, 1, mean)

# make a map of the predictions
obs_and_pred <- modis_temps$subtemp
obs_and_pred[pred_inds] <- pred

par(mfrow=c(1,2))
image.plot(array(modis_temps$subtemp, temp_dims), axes=FALSE)
mtext("Data")
image.plot(array(obs_and_pred, temp_dims), axes=FALSE)
mtext("Conditional Expectation (Prediction)")

#### Code for running the ManyGLM analysis for the MS on wildlife responses to wildfire ####

## load the mvabund library 
library(mvabund)

## load the data
load("Wildfire-Data.RData")
attach(data)

## Set seed to get reproducible results
set.seed(123)

## Create an mvabund object from the species observations
many <- mvabund(species.observations)

## Run the first model presented in the manuscript with treatment (burnt vs unburnt control), location (three locations), 
## their interaction as covariates, and placement type as covariates.

## First test if Poisson or Negative Binomial distribution fits best
# Poisson model
manyglm1.p <- manyglm(many ~ covariates$location * covariates$treatment + covariates$placement, family = "poisson", offset = offset, show.coef = T)
# Negative binomial model
manyglm1.nb <- manyglm(many ~ covariates$location * covariates$treatment + covariates$placement, family = "negative.binomial", offset = offset, show.coef = T)
## testing which distribution fits best
anova(manyglm1.p,manyglm1.nb)
plot(manyglm1.p) #residual plot
plot(manyglm1.nb)
# No statistical difference and plot for Negative binomial looks better so continue with that model.

## Check both multivariate and univariate results for individual species
anova(manyglm1.nb, p.uni="adjusted", resamp="perm.resid")

## Run the second model presented in the manuscript with Terrain Ruggedness Index, Number of dead trees (log10), 
## and Number of young trees (log10) as covariates.

## First test if Poisson or Negative Binomial distribution fits best
# Poisson model
manyglm2.p <- manyglm(many ~ covariates$rug + covariates$logdead + covariates$logtrees, family = "poisson", offset = offset, show.coef = T)
# Negative binomial model
manyglm2.nb <- manyglm(many ~ covariates$rug + covariates$logdead + covariates$logtrees, family = "negative.binomial", offset = offset, show.coef = T)
## testing which distribution fits best
anova(manyglm2.p,manyglm2.nb)
plot(manyglm2.p) #residual plot
plot(manyglm2.nb)
# Negative binomial statistically supported and looks better so continue with that model.

## Check both multivariate and univariate results for individual species
anova(manyglm2.nb, p.uni="adjusted", resamp="perm.resid")


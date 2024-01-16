#### Code for running the ManyGLM analysis for the MS on wildlife responses to wildfire ####

## load the mvabund library 
library(mvabund)

## load the data
data <- read.csv("Data.csv")

## Create a dataframe with only the visitation frequencies of the species
spe <- subset(data, select = -c(deployment_id, treatment, location_id, location_t))

## Create a corresponding dataframe for detection / non-detection of the species
spe_pa <- spe
spe_pa[spe_pa > 0] <- 1

## Set seed to get reproducible results
set.seed(2)

## Create an mvabund object from the species visitation frequencies
many <- mvabund(spe)

## Run the model presented in the manuscript with treatment (burnt vs unburnt control), location (three locations), 
## and their interaction as covariates
manyglm <- manyglm (many ~ data$treatment * data$location_id, family = "negative.binomial")

## Check both multivariate and univariate results for individual species
anova(manyglm, p.uni="adjusted", resamp="perm.resid") 

## Create an mvabund object from the species detection / non-detection data
many2 <- mvabund(spe_pa) 

## Run the model presented in teh supplementary files with treatment, location, and their interaction as covariates
manyglm2 <- manyglm (many2 ~ data$treatment * data$location_id, family = "binomial")

## Check both multivariate and univariate results for individual species
anova(manyglm2, p.uni="adjusted", resamp="montecarlo")

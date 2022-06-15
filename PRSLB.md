---
title: "Variation in ontogenetic trajectories of limb dimensions in humans is attributable to both climatic effects and neutral evolution"
author: "An-Di Yim, Libby Cowgill, David C. Katz, Charles C. Rosem"
output: html_document
    toc: yes
    toc_depth: '2'
---

```{r setup, include=FALSE}
  
  knitr::opts_chunk$set(echo = TRUE)
  knitr::opts_chunk$set(message = FALSE)
  knitr::opts_chunk$set(warning = FALSE)

```

# Get climatic variables with WorldClim and PaleoClim

First, get climatic variables from WorldClim database:

```{r worldClim}
library(raster)
library(rgdal)

# For contemporary climatic variables, download WorldClim 
# Use var = "bio" to download bioclimatic variables
current <- getData('worldclim', var='bio', res = 2.5)

# I only want annual mean temp. (1), max temp. of warmest month (5), and min temp. of coldest month (6)
current <- current[[c(1, 5, 6)]]

# Check raster layers were extracted correctly
plot(current)

# Get latitudes and longitudes of different populatios 
# The following coordinates correspod to: France, South Africa, Taiwan, Netherlands, the United States (New Mexico), and Columbia
#Denmark lat: 55.674, lon:12.570

current_lats <- c("43.297", "-33.954", "25.033", "52.289", "35.084", "6.267")
current_lons <- c("5.381", "18.488", "121.517", "4.955","-106.619", "-75.568")

current_lats <- as.numeric(current_lats)
current_lons <- as.numeric(current_lons)
geographic_coords <- data.frame(x = current_lons,y = current_lats)

# Extract data point from raster layers
current_points <- SpatialPoints(geographic_coords, proj4string = current@crs)

current_values <- extract(current, current_points)

contemporary.climate <- cbind.data.frame(coordinates(current_points), current_values)

# And these are the temporature readings in degree Celcius:
contemporary_pop_temp <- matrix(c(contemporary.climate$bio1/10,
         contemporary.climate$bio5/10,
         contemporary.climate$bio6/10), nrow = 3, ncol = 6, byrow = TRUE,
         dimnames = list(c("Annual Temp.", "Warmest Temp.", "Coldest Temp."), 
                         c("France", "South Africa", "Taiwan", "Netherlands", "United States (New Mexico)", "Columbia")))

contemporary_pop_temp

```

Then, get climatic variables from PaleoClim database:

```{r PaleoClim}
# This data is available for download at: http://www.paleoclim.org/

# Mid-Holocene bioclimatic variable layers
MH_raster <- stack(list.files(path = file.path(fullpath, "./Mid_Holocene"), full.names = TRUE, pattern = ".tif")[seq(1, 95, by =5)])

# Late Holocene bioclimatic variable layers
LH_raster <- stack(list.files(path = file.path(fullpath, "./Late_Holocene"), full.names = TRUE, pattern = "tif")[seq(1, 95, by =5)])

# Only want bioclimatic variables 1 (mean annual temp), 5 (max temp of warm month) and 6 (min temp of cold month)
## Sequence of bioclimatic variables: 1, 10-19, 2, 3, ...9
## Therefore 5 and 6 are the 15th and 16th variable in this raster layer
MH_raster <- MH_raster[[c(1, 15, 16)]]
LH_raster <- LH_raster[[c(1, 15, 16)]]

par(mfrow = c(2, 1))
plot(MH_raster)
plot(LH_raster)

# Get latitudes and longitudes of past populations
# The following populations were mid-Holocene: Amerindians and Indian Knoll
MHlats <- c(40.80, 37.268)
MHlons <- c(-123.80, -86.984)

# The rest of past populations were from late Holocene
# Amerindians also cross over to late Holocene
# So the following are: Amerindians, Kulubnarti, Mistihalj, Dart, Luis Lopes, and Point Hope
LHlats <- c(40.80, 21.070, 43.704, -26.188, 38.717, 68.341)
LHlons <- c(-123.80, 30.665, 19.396, 28.025, -9.133, -166.758)

MH.coords <- data.frame(x=MHlons, y=MHlats)
LH.coords <- data.frame(x=LHlons, y=LHlats)

# Repeat the same process as WorldClim data
MH.points <- SpatialPoints(MH.coords, proj4string = MH_raster@crs)
LH.points <- SpatialPoints(LH.coords, proj4string = LH_raster@crs)

MH.values <- extract(MH_raster, MH.points)
LH.values <- extract(LH_raster, LH.points)

MH <- cbind.data.frame(coordinates(MH.points), MH.values)
LH <- cbind.data.frame(coordinates(LH.points), LH.values)

# And these are the temporature readings in degree Celcius:
# For mid-Holocene:
midholocene_pop_temp <- matrix(c(MH$bio_1/10,
         MH$bio_5/10,
         MH$bio_6/10), nrow = 3, ncol = 2, byrow = TRUE,
         dimnames = list(c("Annual Temp.", "Warmest Temp.", "Coldest Temp."), 
                         c("Amerindians", "Indian Knoll")))
midholocene_pop_temp

# For late Holocene:
lateholocene_pop_temp <- matrix(c(LH$bio_1/10,
         LH$bio_5/10,
         LH$bio_6/10), nrow = 3, ncol = 6, byrow = TRUE,
         dimnames = list(c("Annual Temp.", "Warmest Temp.", "Coldest Temp."), 
                         c("Amerindians", "Kulubnarti", "Mistihalj", "Dart", "Luis Lopes", "Point Hope")))

lateholocene_pop_temp

```

# Mixed model approach to growth trajectories

First, we need a population relationship matrix. To do that, we need a matrix of distances between populations.

```{r mixed model set up}

# This is the input format for David's function
all_pop <- as.data.frame(matrix(c(40.80, -123.80, "Cal Amerindian", "NAmerica",
         37.268, -86.984, "Indian Knoll", "NAmerica",
         21.070, 30.665, "Kulubnarti", "Africa",
         43.704, 19.396, "Mistihalj", "Europe",
         -26.188, 28.025, "Dart", "Africa",
         38.717, -9.133, "Luis Lopes", "Europe",
         68.341, -166.758, "Point Hope", "NAmerica",
         43.297, 5.381, "FR", "Europe",
         -33.954, 18.488, "SA", "Africa",
         25.033, 121.517, "TW", "Asia",
         52.289, 4.955, "NL", "Europe",
         35.084, -106.619, "US", "NAmerica",
         6.267, -75.568, "COL", "SAmerica"), ncol = 4, byrow = TRUE))

all_pop[,1] <- as.numeric(as.character(all_pop[,1]))
all_pop[,2] <- as.numeric(as.character(all_pop[,2]))
names(all_pop) <- c("Latitude", "Longitude", "Pop", "Region")

# Save a csv file for these data for the future
write.csv(all_pop, file = "all_pop_info.csv", row.names = FALSE)

# Use distance.overland function to calculate a matrix of geographic distances
# This function is available at https://gist.github.com/davidckatz
dist.mat <- distance.overland(all_pop)
dist.mat

# Once we have our geographic distances, use the following regression coefficients to get the estimated delta mus
# These coefficients are based on regression of genetic distances (CEPH database) on geographic distances
my.int <- 4.5466402644      
my.beta <- 0.0008127043

# Predict delta mu squared with linear regression
linear.dmu <- my.int + (my.beta * dist.mat)
diag(linear.dmu) <- 0
#pred.dmu.distmat <- as.dist(linear.dmu)
# CALCULATE RELATIONSHIP (A) MATRIX BASED ON PAIRWISE DELTA-MU VALUES
# This function is also available at https://gist.github.com/davidckatz
Amat.linear <- Dmax.Amatrix(linear.dmu)

```

The A matrix is analogous to relationship matrix used in phylogenetic analysis. The elements Aij in the A matrix are equal to the length of the path from the most recent common ancestor of species i and j to the root of the phylogeny. ( _Reference:_ Hadfield, J. D., & Nakagawa, S. (2010). General quantitative genetic methods for comparative biology: phylogenies, taxonomies and multi‐trait models for continuous and categorical characters. *Journal of evolutionary biology*, 23(3), 494-508.)

Since we populate the A matrix __*directly*__, we are now ready to conduct the mixed model analysis.

## Mixed model analysis usig `brms` package

*Reference:* Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel models using Stan. *Journal of Statistical Software*, 80(1), 1-28.

*Reference:* Lüdecke, D. (2018). sjstats: Statistical functions for regression models. R package version 0.14, 3.


```{r model fitting}

library(brms)
library(rstan)
library(sjstats)
library(sjmisc)
library(qtlmt)

# I am generating 1 MCMC chain, with 500000 iterations and 250000 burn-in, thinning rate is 250
# Femur null models:
femur.null_1 <- brm(mvbind(FDL, FMSB, FDB) ~ sqrt(Age) + Weight + Cold.Temp + Type, data = femur,
                  family = list("gaussian", "gaussian", "gaussian"),
                  chains = 1, iter = 500000, warmup = 250000, thin = 250)

femur.null_2 <- brm(mvbind(FDL, FMSB, FDB) ~ sqrt(Age) + Weight + Warm.Temp + Type, data = femur,
                  family = list("gaussian", "gaussian", "gaussian"),
                  chains = 1, iter = 500000, warmup = 250000, thin = 250)

femur.null_3 <- brm(mvbind(FDL, FMSB, FDB) ~ sqrt(Age) + Weight + Ann.Temp + Type, data = femur,
                  family = list("gaussian", "gaussian", "gaussian"),
                  chains = 1, iter = 500000, warmup = 250000, thin = 250)

femur.null_4 <- brm(mvbind(FDL, FMSB, FDB) ~ sqrt(Age) + Weight + Cold.Temp + Warm.Temp + Type, data = femur,
                    family = list("gaussian", "gaussian", "gaussian"),
                    chains = 1, iter = 500000, warmup = 250000, thin = 250)

femur.null_5 <- brm(mvbind(FDL, FMSB, FDB) ~ sqrt(Age) + Weight + Cold.Temp + Ann.Temp + Type, data = femur,
                    family = list("gaussian", "gaussian", "gaussian"),
                    chains = 1, iter = 500000, warmup = 250000, thin = 250)

femur.null_6 <- brm(mvbind(FDL, FMSB, FDB) ~ sqrt(Age) + Weight + Warm.Temp + Ann.Temp + Type, data = femur,
                    family = list("gaussian", "gaussian", "gaussian"),
                    chains = 1, iter = 500000, warmup = 250000, thin = 250)

femur.null_7 <- brm(mvbind(FDL, FMSB, FDB) ~ sqrt(Age) + Weight + Cold.Temp + Warm.Temp + Ann.Temp + Type, data = femur,
                     family = list("gaussian", "gaussian", "gaussian"),
                     chains = 1, iter = 500000, warmup = 250000, thin = 250)

# Model comparison
femur.null_1 <- add_criterion(femur.null_1, criterion = "waic")
femur.null_1 <- add_criterion(femur.null_1, criterion = "loo")

femur.null_2 <- add_criterion(femur.null_2, criterion = "waic")
femur.null_2 <- add_criterion(femur.null_2, criterion = "loo")

femur.null_3 <- add_criterion(femur.null_3, criterion = "waic")
femur.null_3 <- add_criterion(femur.null_3, criterion = "loo")

femur.null_4 <- add_criterion(femur.null_4, criterion = "waic")
femur.null_4 <- add_criterion(femur.null_4, criterion = "loo")

femur.null_5 <- add_criterion(femur.null_5, criterion = "waic")
femur.null_5 <- add_criterion(femur.null_5, criterion = "loo")

femur.null_6 <- add_criterion(femur.null_6, criterion = "waic")
femur.null_6 <- add_criterion(femur.null_6, criterion = "loo")

femur.null_7 <- add_criterion(femur.null_7, criterion = "waic")
femur.null_7 <- add_criterion(femur.null_7, criterion = "loo")


loo_compare(femur.null_1, femur.null_2, femur.null_3,
            femur.null_4, femur.null_5, femur.null_6,
            femur.null_7, criterion = "waic")

loo_compare(femur.null_1, femur.null_2, femur.null_3,
            femur.null_4, femur.null_5, femur.null_6,
            femur.null_7, criterion = "loo")

bayes_R2(femur.null_1)
bayes_R2(femur.null_2)
bayes_R2(femur.null_3)
bayes_R2(femur.null_4)
bayes_R2(femur.null_5)
bayes_R2(femur.null_6)
bayes_R2(femur.null_7)

# Femur pop structure only model

femur.no.climate <- brm(mvbind(FDL, FMSB, FDB) ~ sqrt(Age) + Weight + Type + (1|2|Sample), 
                        data = femur,
                        cov_ranef = list(Sample = Amat.linear), 
                        family = list("gaussian", "gaussian", "gaussian"),
                        chains = 1, iter = 500000, warmup = 250000, thin = 250,
                        backend = "cmdstanr", threads = threading(8))


# Femur inclusive model

femur_i <- brm(mvbind(FDL, FMSB, FDB) ~ sqrt(Age) + Weight + Cold.Temp + Warm.Temp + Ann.Temp + Type + (1|2|Sample), 
                    data = femur, cov_ranef = list(Sample = Amat.linear),
                 family = list("gaussian", "gaussian", "gaussian"),
                 chains = 4, iter = 500000, warmup = 250000, thin = 250, cores = 4, control = list(adapt_delta = 0.99))

femur_i <- add_criterion(femur_i, "loo")
femur_i <- add_criterion(femur_i, "waic")

# Femur inclusive random slope model

femur_is <- brm(mvbind(FDL, FMSB, FDB) ~ sqrt(Age) + Weight + Cold.Temp + Warm.Temp + Ann.Temp + Type + (1 + sqrt(Age)|2|Sample), 
                    data = femur, cov_ranef = list(Sample = Amat.linear),
                    family = list("gaussian", "gaussian", "gaussian"),
                    chains = 1, iter = 500000, warmup = 250000, thin = 250,
                backend = "cmdstanr", threads = threading(8), control = list(adapt_delta = 0.99))

femur_is <- add_criterion(femur_is, "loo")
femur_is <- add_criterion(femur_is, "waic")



```
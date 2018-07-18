#https://cran.r-project.org/web/packages/ENMeval/vignettes/ENMeval-vignette.html

library(spocc)
library(ENMeval)
library(raster)
library(dismo)

#####################################################
#### Data Acquisition & Pre-processing #####
#####################################################

# Search GBIF for occurrence data.
bv <- occ('Bradypus variegatus', 'gbif', limit=300, has_coords=TRUE)

# Get the latitude/coordinates for each locality. Also convert the tibble that occ() outputs
# to a data frame for compatibility with ENMeval functions.
occs <- as.data.frame(bv$gbif$data$Bradypus_variegatus[,2:3])

# Remove duplicate rows (Note that you may or may not want to do this).
occs <- occs[!duplicated(occs),]

# First, load some predictor rasters from the dismo folder:
files <- list.files(path=paste(system.file(package='dismo'), '/ex', sep=''), pattern='grd', full.names=TRUE)

# Put the rasters into a RasterStack:
envs <- stack(files)

# Plot first raster in the stack, bio1
plot(envs[[1]], main=names(envs)[1])

# Add points for all the occurrence points onto the raster
points(occs)

# There are some points all the way to the south-east, far from all others. Let's say we know that this represents a subpopulation that we don't want to include, and want to remove these points from the analysis. We can find them by first sorting the occs table by latitude.
head(occs[order(occs$latitude),])
#>     longitude  latitude
#> 21  -43.40630 -23.00829
#> 83  -43.23764 -22.93862
#> 110 -63.16667 -17.78333
#> 1   -63.06480 -17.77664
#> 78  -63.17032 -17.74494
#> 2   -63.65988 -17.46079

# We see there are two such points, and we can find them by specifying a logical statement that says to find all records with latitude less than -20.
index <- which(occs$latitude < (-20))

index <- which(occs$longitude > (-50))
# Next, let's subset our dataset to remove them by using the negative assignment on the index vector.
occs <- occs[-index,]

# Let's plot our new points over the old ones to see what a good job we did.
points(occs, col='blue')

# Make a SpatialPoints object
occs.sp <- SpatialPoints(occs)

# Get the bounding box of the points
bb <- bbox(occs.sp)

# Add 5 degrees to each bound by stretching each bound by 10, as the resolution is 0.5 degree.
bb.buf <- extent(bb[1]-10, bb[3]+10, bb[2]-10, bb[4]+10)

# Crop environmental layers to match the study extent
envs.backg <- crop(envs, bb.buf)

library(maptools)
library(rgeos)

# Get a simple world countries polygon
data(wrld_simpl)

plot(wrld_simpl)

# Get polygons for Central and South America
ca.sa <- wrld_simpl[wrld_simpl@data$SUBREGION==5 | wrld_simpl@data$SUBREGION==13,]

# Both spatial objects have the same geographic coordinate system with slightly different specifications, so just name the coordinate reference system (crs) for ca.sa with that of
# envs.backg to ensure smooth geoprocessing.
crs(envs.backg) <- crs(ca.sa)

# Mask envs by this polygon after buffering a bit to make sure not to lose coastline.
ca.sa <- gBuffer(ca.sa, width=1)
envs.backg <- mask(envs.backg, ca.sa)

# Let's check our work. We should see Central and South America without the Carribbean.
plot(envs.backg[[1]], main=names(envs.backg)[1])
points(occs)

# Randomly sample 10,000 background points from one background extent raster (only one per cell without replacement). Note: Since the raster has <10,000 pixels, you'll get a warning and all pixels will be used for background.
bg <- randomPoints(envs.backg[[1]], n=10000)
bg <- as.data.frame(bg)

# Notice how we have pretty good coverage (every cell).
plot(envs.backg[[1]], legend=FALSE)
points(bg, col='red')

#####################################################
#### Set up MaxEnt ####
#####################################################

#Please download Maxent here: http://www.cs.princeton.edu/~schapire/maxent/"
#Put the file 'maxent.jar' in the 'java' folder of this package. 
#That is the folder returned by system.file("java", package="dismo")

#####################################################
#### Running ENMeval ####
#####################################################

#The main parameters to define when calling ENMevaluate are:
# method = method for partitioning occurrences for evaluation
#RMvalues = the range of regularization multiplier values and 
#fc = the combinations of feature class to consider ( L=linear, Q=quadratic, P=product,T=threshold, and H=hinge) 
#If any of your predictor variables are categorical (e.g., biomes), 
#you will need to define which layer(s) these are in the 'categoricals' argument

#We will use Checkerboard2 (other methods are available)
#check2 <- get.checkerboard2(occs, envs, bg, aggregation.factor=c(5,5))
#plot(envs.backg[[1]], col='gray', legend=FALSE)
#points(bg, pch=21, bg=check2$bg.grp)
#points(occs, pch=21, bg=check2$occ.grp, col='white', cex=1.5)

eval2 <- ENMevaluate(occ=occs, env=envs, bg.coords=bg, method='checkerboard2', RMvalues=c(1,2), fc=c('L','LQ','LQP'))

#or running the command in parallel for big projects
eval2.par <- ENMevaluate(occs, envs, bg, method='checkerboard2', RMvalues=c(1,2), fc=c('L','LQ','LQP'), parallel=TRUE)

#Compare with maxent function from dismo
#me <- maxent(envs, occs, factors='biome', args=c('hinge=false', 'threshold=false'))


#Exploring the results
eval2
str(eval2, max.level=3)

#Evaluation metrics
eval2@results

#Use AICc  to find 'best' model
eval2@results[which(eval2@results$delta.AICc==0),]

#Predictions
eval2@predictions

#Now plot the model with the lowest AICc:
plot(eval2@predictions[[which(eval2@results$delta.AICc==0)]], main="Relative occurrence rate")

#We can also access a list of Maxent model objects, which (as all lists) can be subset with double brackets (e.g. results@eval2[[1]]).
#The Maxent model objects provide access to various elements of the model (including the lambda file).
#The model objects can also be used for predicting models into other time periods or geographic areas.
#Let's look at the model object for our "AICc optimal" model:
aic.opt <- eval2@models[[which(eval2@results$delta.AICc==0)]]
aic.opt
#The "results" slot shows the Maxent model statistics:
aic.opt@results

var.importance(aic.opt)

#Check .lambdas file
aic.opt@lambdas

#this is not very structured. Therefore we?ll use parse_lambdas() from rmaxent

#Load rmaxent package
library(devtools)
install_github('johnbaums/rmaxent')
library(rmaxent)

#This package has the following nice functions:
#project() takes a trained Maxent model and predicts it to new data
#parse_lambdas(): extract info from the .lambdas file
#limiting(): To identify which variable is most limiting in a certain region

#See lambda file. First column is coefficient
parse_lambdas(aic.opt)

#To identify which variable is most responsible for decreasing suitability in a given environment,
#we can use the limiting function.
lim <- limiting(envs, aic.opt)
levelplot(lim, col.regions=rainbow) +
  layer(sp.points(SpatialPoints(occs), pch=20, col=1))

#####################################################
#### Plotting results ####
#####################################################
#Plot evaluation of different results
eval.plot(eval2@results) #deltaAICc is default
eval.plot(eval2@results, 'Mean.AUC', var='Var.AUC')
df <- var.importance(aic.opt)
barplot(df$permutation.importance, names.arg=df$variable, las=2, ylab="Permutation Importance")

#Plot the model with the lowest AICc
plot(eval2@predictions[[which(eval2@results$delta.AICc==0)]], main="Relative occurrence rate")

#Compare model predictions
par(mfrow=c(1,2), mar=c(2,2,1,0))

plot(eval2@predictions[['L_2']], ylim=c(-30,20), xlim=c(-90,-40), legend=F, main='L_2 prediction')

plot(eval2@predictions[['LQP_1']], ylim=c(-30,20), xlim=c(-90,-40), legend=F, main='LQP_1 prediction')

#Plotting response curves of the different variables
response(eval2@models[[1]])

#####################################################
#### Project onto new data ####
#####################################################
prediction <- project(aic.opt, envs)

#plot the result:
library(rasterVis)
library(viridis)
levelplot(prediction$prediction_logistic, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100)) +
  layer(sp.points(SpatialPoints(occs), pch=20, col=1))


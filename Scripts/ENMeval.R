library(spocc)
library(raster)
library(dismo)
library(rJava)
library(ENMeval)
install.packages("rJava")
.jinit()

#Set up MaxEnt ####

#Please download Maxent here: http://www.cs.princeton.edu/~schapire/maxent/"
#Put the file 'maxent.jar' in the 'java' folder of this package. 
#That is the folder returned by system.file("java", package="dismo")

#Running ENMeval ####

#we prepare the current climatic data raster again by clipping them to Europe
setwd("E:/Thesis/ClimaticData/bio1-19_30s_bil/")
files <- list.files(pattern='\\.bil$') #import tif files- change the pattern to suit the type of file
predictor_19 <- stack(files)
setwd("C:/Users/user/Documents/School/Thesis/Script/RedList")
Europe <- shapefile('C:/Users/user/Documents/School/Thesis/Script/RedList/Europe_shp.shp')
ext <- extent(-31.29, 40.17972, 27.6375, 71.15471)
crs(Europe) <- "+proj=longlat +datum=WGS84 +no_defs"
predictor_19 <- crop(predictor_19, ext)
predictor_19 <- mask(predictor_19, Europe) # crop the extent to Europe
crs(predictor_19) <- "+proj=longlat +datum=WGS84 +no_defs" #set projection to WGS84
plot(predictor_19[[1]])
predictor_n <- predictor_19[[1:3]]
setwd("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/CleanData")
files_cleanData <- list.files(pattern = "\\.csv$")

#Start of ENMeval
for (r in 1:length(files_cleanData)){
  species_of_interest <- read.csv(files_cleanData[[r]], header = TRUE, sep = ",")
  species_of_interest <- species_of_interest[, 2:4]
  setwd("C:/Users/user/Documents/School/Thesis/Script/RedList")
}  

#we use the blocked method and the feature classes "linear", "quadratic", "product".
eval2 <- ENMevaluate(occ= species_of_interest[,2:3], n.bg = 10000, env= predictor_n, method='block', RMvalues= seq(1, 2, 1), fc=c('L', 'LQ', 'LQP'))

#Exploring the results and finding the 'best' model
eval2@results
AICc_0 <- eval2@results[which(eval2@results$delta.AICc==0),]

#Predictions and plotting the model with the lowest AICc
eval2@predictions
plot(eval2@predictions[[which(eval2@results$delta.AICc==0)]], main="Relative occurrence rate", las=1) #this will give the ROR or relative occurrence rate (map with the many different colours, but very small values in the legend)

#We can also access a list of Maxent model objects, which (as all lists) can be subset with double brackets (e.g. results@eval2[[1]]).
#The Maxent model objects provide access to various elements of the model (including the lambda file).
#The model objects can also be used for predicting models into other time periods or geographic areas.
#Let's look at the model object for our "AICc optimal" model:
aic.opt <- eval2@models[[which(eval2@results$delta.AICc==0)]] #can't find the optimal because there are severeal with a delta AICc equal to 0
aic.opt

#The "results" slot shows the Maxent model statistics:
aic.opt@results

#get a data.frame of two variable importance metrics: percent contribution and permutation importance.
var.importance(aic.opt)

#Check .lambdas file
aic.opt@lambdas

#this is not very structured. Therefore we?ll use parse_lambdas() from rmaxent

#Load rmaxent package
install.packages("devtools")
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

library(lattice)

lim <- limiting(envs, aic.opt)
levelplot(lim, col.regions=rainbow) +
  layer(sp.points(SpatialPoints(occs), pch=20, col=1)) #won't work

#####################################################
#### Plotting results ----
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

#plotting (Scott) ####
eval2_curr_logistic <- predict(eval2@models[[1]], predictor_n, args = c("outputformat=logistic"))
#eval2_curr_logistic <- predict(eval2@models[[1]], predictor_n, args = c("outputformat=logistic"))
plot(eval2_curr_logistic, main = "Logistic")
setwd("C:/Users/user/Documents/School/Thesis/Results/00_current_continuous")
dev.copy2pdf(file="Gentiana pneumonanthe.pdf", width = 7, height = 5) #important to put the extent in file!!
writeRaster(x = eval2_curr_logistic, filename = "continuous", overwrite = TRUE, format = "GTiff")
setwd("C:/Users/user/Documents/School/Thesis/Script/RedList")

eval2_aic_opt <- eval2@results[which(eval2@results$delta.AICc == 0), ]
eval2_curr_threshold_OR10 <- eval2_curr_logistic > eval2_aic_opt[, "Mean.OR10"]
plot(eval2_curr_threshold_OR10, main = "Threshold 10%")
setwd("C:/Users/user/Documents/School/Thesis/Results/01_current_threshold_10%")
dev.copy2pdf(file="Gentiana pneumonanthe.pdf", width = 7, height = 5) #important to put the extent in file!!
writeRaster(x = predictor_19[[1]], filename = "predictor_clipped", overwrite = TRUE, format = "GTiff")
setwd("C:/Users/user/Documents/School/Thesis/Script/RedList")

eval2_curr_threshold_ORmin <- eval2_curr_logistic > eval2_aic_opt[, "Mean.ORmin"]
plot(eval2_curr_threshold_ORmin, main = "Threshold min")
setwd("C:/Users/user/Documents/School/Thesis/Results/02_current_threshold_min")
dev.copy2pdf(file="Gentiana pneumonanthe.pdf", width = 7, height = 5) #important to put the extent in file!!
writeRaster(x = predictor_19[[1]], filename = "predictor_clipped", overwrite = TRUE, format = "GTiff")
setwd("C:/Users/user/Documents/School/Thesis/Script/RedList")

#Project onto future data #####

#we prepare the future climatic data raster again by clipping them to Europe
setwd("E:/Thesis/ClimaticData/FutureClimate(HadGEM2-ES, RCP8.5)/")
files_futureClimate <- list.files(pattern='\\.tif$') #import tif files- change the pattern to suit the type of file
predictor_19_future <- stack(files_futureClimate)
setwd("C:/Users/user/Documents/School/Thesis/Script/RedList")
Europe <- shapefile('C:/Users/user/Documents/School/Thesis/Script/RedList/Europe_shp.shp')
ext <- extent(-31.29, 40.17972, 27.6375, 71.15471)
crs(Europe) <- "+proj=longlat +datum=WGS84 +no_defs"
predictor_19_future <- crop(predictor_19_future, ext)
predictor_19_future <- mask(predictor_19_future, Europe) # crop the extent to Europe
crs(predictor_19_future) <- "+proj=longlat +datum=WGS84 +no_defs" #set projection to WGS84
plot(predictor_19_future[[1]])

eval2_future_logistic <- predict(eval2@models[[1]], predictor_19_future, args = c("outputformat=logistic"))
plot(eval2_future_logistic)

#Sidenotes ####

#running the command in parallel for big projects:
#eval2.par <- ENMevaluate(occs, envs, bg, method='checkerboard2', RMvalues=c(1,2), fc=c('L','LQ','LQP'), parallel=TRUE)

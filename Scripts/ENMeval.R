library(spocc)
library(raster)
library(dismo)
library(rJava)
library(ENMeval)


#### Set up MaxEnt ####

#Please download Maxent here: http://www.cs.princeton.edu/~schapire/maxent/"
#Put the file 'maxent.jar' in the 'java' folder of this package. 
#That is the folder returned by system.file("java", package="dismo")

#### Running ENMeval ####

#The main parameters to define when calling ENMevaluate are:
#method = method for partitioning occurrences for evaluation (we will use 'block' method)
#RMvalues = the range of regularization multiplier values and 
#fc = the combinations of feature class to consider ( L=linear, Q=quadratic, P=product,T=threshold, and H=hinge) 

setwd("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/CleanData")
files_cleanData <- list.files(pattern = "\\.csv$")

for (i in length(files_cleanData)){
  species_of_interest <- read.csv(files_cleanData, header = TRUE, sep = ",")
  species_of_interest <- species_of_interest[, 2:4]
  setwd("C:/Users/user/Documents/School/Thesis/Script/RedList")
}  

predictor_n <- predictor_19[[1:3]]

eval2 <- ENMevaluate(occ= species_of_interest[,2:3], n.bg = 10000, env= predictor_n, method='block', RMvalues=seq(1, 2, 0.5), fc=c('L', 'LQ', 'LQP'))

#eval2 <- ENMevaluate(occ=sp1[,2:3], n.bg = 20000, env=predictors, method='block', RMvalues=seq(0.5, 2, 0.5), fc=c('L', 'LQ', 'LQP'))


#or running the command in parallel for big projects
#eval2.par <- ENMevaluate(occs, envs, bg, method='checkerboard2', RMvalues=c(1,2), fc=c('L','LQ','LQP'), parallel=TRUE)

#Compare with maxent function from dismo
#me <- maxent(envs, occs, factors='biome', args=c('hinge=false', 'threshold=false'))


#Exploring the results
eval2
str(eval2, max.level=3)

#Evaluation metrics
eval2@results

#Use AICc  to find 'best' model
AICc_0 <- eval2@results[which(eval2@results$delta.AICc==0),]
AICc_0

#Predictions
eval2@predictions

#Now plot the model with the lowest AICc:
plot(eval2@predictions[[which(eval2@results$delta.AICc==0)]], main="Relative occurrence rate")

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

#####################################################
#### Project onto new data ----
#####################################################
prediction <- project(aic.opt, envs)

#plot the result:
install.packages("rasterVis")
library(rasterVis)
install.packages("viridis")
library(viridis)
levelplot(prediction$prediction_logistic, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100)) +
  layer(sp.points(SpatialPoints(occs), pch=20, col=1))


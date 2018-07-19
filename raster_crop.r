#Tuatara ENM
#Author Scott Jarvie

#load libraries ---
install.packages("pacman", repos="https://cloud.r-project.org")#only need to do once
pacman::p_load(raster,
               tidyverse, update = F)

# import climate data -----
setwd("") # set working directory
files <- list.files(pattern='\\.tif$') #import tif files- change the pattern to suit the type of file
predictors <- stack(files) # 
ext <- extent(166,179,-48,-33.5) # this is the extent for NZ; modify for Europe
predictors <- crop(predictors, ext) # crop the extent to NZ
#crs(predictors) <- "+proj=longlat +datum=WGS84 +no_defs" # set projection if you want
plot(predictors[[1]]) #have a look at the crop
plot(predictors[[6]])

setwd("C:/Users/au596783/Desktop/nz/") # set working directory where you want to write the files - will save as tiff files
writeRaster(x = predictors, names(predictors), overwrite = TRUE, format = "GTiff", bylayer = TRUE)

#check if it works by importing then looking at them
setwd("C:/Users/au596783/Desktop/nz/") # set working directory
nz_files <- list.files(pattern='\\.tif$') #import tif files
nz_predictors <- stack(nz_files)

plot(nz_predictors) #all files
plot(nz_predictors[[6]]) #just one
#install.packages("CoordinateCleaner")
#install.packages("scrubr")
#install.packages("biogeo")
#install.packages("raster")
#install.packages("rgbif")
#install.packages("dismo")
#install.packages("mapr")
library(CoordinateCleaner)
library(scrubr)
library(biogeo) 
library(raster)
library(dismo)
library(mapr)

setwd("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/Occurrence_data_spocc")
files_occurrence <- list.files(pattern = "\\.csv$")

for (i in length(files_occurrence)){
clean0 <- read.csv(files_occurrence[[i]], header = TRUE, sep = ",")
clean0 <- clean0[, 2:8]

setwd("C:/Users/user/Documents/School/Thesis/Script/RedList")

#Step 1: Removal of duplicates ####

clean1 <- cc_dupl(clean0, lon = "x_original", lat = "y_original", species = "databaseName", additions = NULL, verbose = TRUE)

clean1 <- cc_equ(clean1, lon = "x_original", lat = "y_original", test = "identical", verbose = TRUE) #additional test to be sure that all duplicates are really gone. Additionally checks on identical coordinates.

#Step2: Remove impossible, incomplete, unlikely coordinates ####

clean2 <- coord_incomplete(clean1, lat = "y_original", lon = "x_original", drop = TRUE) #remove incomplete coordinates (removal of rows which only consist of NA's)

clean2 <- coord_impossible(clean2, lat = "y_original", lon = "x_original", drop = TRUE) #remove impossible coordinates #will normally give the same result after the cleaning steps which have already be done, just a built in security, function won't work

clean2 <- coord_unlikely(clean2, lat = "y_original", lon = "x_original", drop = TRUE) #remove unlikely coordinates (eg 0,0) which are indicators for erroneous data and high concentrations around equator and prime meridian (Greenwich) 

clean_biological <- CleanCoordinates(clean2, lon = "x_original", lat = "y_original", species = "databaseName") #test if all records are valid (if validity = TRUE --> OK!)

#Step 3: Centroid detection ####

clean3 <- cc_cap(clean2, lon = "x_original", lat = "y_original", verbose = TRUE) #we use a default buffer around each capital of 0.1 degree and the default SpatialPointsDataframe "capitals"
clean3 <- cc_cen(clean3, lon = "x_original", lat = "y_original", test = "country", verbose = TRUE) #country centroids are deleted again with the same defaults as centroid_capital

#Step 4: Spatial resolution (error) ####

clean4 <- subset(clean3, spatialError < 10000 | is.na(clean3$spatialError) == TRUE) #are we also removing the na's?? (Scott is going to ask Jens)

#Step 5: Occurrences based on fossil material, germplasm, literature ####

clean5 <- subset(clean4, !(issues == "gass84") & !(issues == "cdround"))
#discuss which ones we will be deleting, now I just 'tested' these ones

#Step 6: Occurences located near sea/lakes ####

#possible option when we are going to use multiple climatic layers
setwd("E:/Thesis/ClimaticData/bio1-19_30s_bil/")
files <- list.files(pattern='\\.bil$') #import tif files- change the pattern to suit the type of file
predictor_19 <- stack(files)
setwd("C:/Users/user/Documents/School/Thesis/Script/RedList")

plot(predictor_19[[1]])
plot(predictor_19[[6]])

clean5$ID <- seq.int(nrow(clean5)) #format for biogeo
clean5 %>% 
  keepmainfields(ID = 'ID', Species = 'Species', x = 'x_original', y = 'y_original') -> species_arrange_main_fields
clean6 <- nearestcell(dat = species_arrange_main_fields, rst = predictor_19[[1]]) # returns a list
clean6_dat <- clean6$dat #select list

for (i in 1:nrow(clean6_dat)) {
  if (is.na(clean6_dat[i, 5]) == TRUE) { 
    clean6_dat[i,5] <- clean6_dat[i, 3]
  } else { 
    clean6_dat[i,5] <-  clean6_dat[i,5]
  } 
}

for (i in 1:nrow(clean6_dat)) {
  if (is.na(clean6_dat[i, 6]) == TRUE) { 
    clean6_dat[i,6] <- clean6_dat[i, 4]
  } else { 
    clean6_dat[i,6] <-  clean6_dat[i,6]
  } 
}

clean_nearestcell <- dplyr::select(clean6_dat, "Species", "longitude" = "x_original", "latitude" = "y_original")

#Step 7: Second check for duplicates ####
#We check again for duplicates because the nearestcell function may have shifted some of the occurrences to the same location

clean7 <- cc_dupl(clean_nearestcell, lon = "longitude", lat = "latitude", species = "Species", additions = NULL, verbose = TRUE)

clean7 <- cc_equ(clean7, lon = "longitude", lat = "latitude", test = "identical", verbose = TRUE)

#Step 8: Geographical outliers ####

#geo_out <- cc_outl(clean_duplicates1, lon = "longitude", lat = "latitude", species = "Species", value = "clean", verbose = TRUE) #searches geographical outliers by default

#Step 9: Botanical garden + Hyper-anthropogenic environment ####

clean9 <- cc_inst(clean7, lon = "longitude", lat = "latitude", verbose = TRUE)
#anthro <- cc_urb(garden, lon = "longitude", lat = "latitude", verbose = TRUE) no reference provided

#Step 10: Clipping to Europe ####
ext <- extent(-15, 32, 34, 72) # this is the extent for Europe, but only a rough guess. Needs discussion
predictor_19 <- crop(predictor_19, ext) # crop the extent to Europe
crs(predictor_19) <- "+proj=longlat +datum=WGS84 +no_defs" #set projection to WGS84
plot(predictor_19[[1]]) #have a look at the crop 
#[[1]] refers to the layer in the stack

setwd("C:/Users/user/Documents/School/Thesis/Climatic data")
writeRaster(x = predictor_19[[1]], filename = "predictor_clipped", overwrite = TRUE, format = "GTiff", bylayer = TRUE)
setwd("C:/Users/user/Documents/School/Thesis/Script/RedList")

coordinates(clean9) <- ~ longitude + latitude #convert to spatial object
clean10 <- crop(clean9, predictor_19[[1]]) #clean10 is a spatial object, not a dataframe

plot(predictor_19[[1]])
points(clean10, col = "red", pch = 1, cex = 0.5)
clean10_dataframe <- as.data.frame(clean10) #clean10_dataframe is a dataframe

#Step 11: One point per grid cell ####
#We do this to reduce sampling bias (spatial), for example species closer to roads have a much higher change of being sampled causing an overestimation

clean11 <- gridSample(clean10, predictor_19[[1]], n=1) #one point per grid cell, clean11 is a spatial object
clean11_dataframe <- as.data.frame(clean11) #convert to dataframe, but we loose species column?
clean11_dataframe <- data.frame(clean0[1,1], clean11_dataframe)
clean11_dataframe <- dplyr::select(clean11_dataframe, "Species" = "clean0.1..1.", "longitude", "latitude")

plot(predictor_19[[1]])
points(clean11, col = "red", pch = 1, cex = 0.5)

#Step 12: Thresholding ####

if (nrow(clean11_dataframe) < 20) {

} else {
  write.csv(clean11_dataframe, paste("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/CleanData/", clean0[1, 1],".csv", sep=""))
}
}

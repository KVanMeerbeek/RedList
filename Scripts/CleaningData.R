#install.packages("CoordinateCleaner")
#install.packages("scrubr")
#install.packages("biogeo")
#install.packages("raster")
#install.packages("rgbif")
#install.packages("dismo")
library(CoordinateCleaner)
library(scrubr)
library(biogeo) 
library(raster)
library(dismo)

#Removal of duplicates ####

clean_duplicates <- cc_dupl(data, lon = "x_original", lat = "y_original", species = "databaseName", additions = NULL, verbose = TRUE)

clean_duplicates1 <- cc_equ(clean_duplicates, lon = "x_original", lat = "y_original", test = "identical", verbose = TRUE) #additional test to be sure that all duplicates are really gone. Additionally checks on identical coordinates.

#Remove impossible, incomplete, unlikely coordinates ####

clean_incomplete <- coord_incomplete(clean_duplicates1, lat = "y_original", lon = "x_original", drop = TRUE) #remove incomplete coordinates (removal of rows which only consist of NA's)

clean_impossible <- coord_impossible(clean_incomplete, lat = "y_original", lon = "x_original", drop = TRUE) #remove impossible coordinates #will normally give the same result after the cleaning steps which have already be done, just a built in security, function won't work

clean_unlikely <- coord_unlikely(clean_impossible, lat = "y_original", lon = "x_original", drop = TRUE) #remove unlikely coordinates (eg 0,0) which are indicators for erroneous data and high concentrations around equator and prime meridian (Greenwich) 

clean_biological <- CleanCoordinates(clean_unlikely, lon = "x_original", lat = "y_original", species = "databaseName") #test if all records are valid (if validity = TRUE --> OK!)

#Centroid detection ####

centroid_capital <- cc_cap(clean_unlikely, lon = "x_original", lat = "y_original", verbose = TRUE) #we use a default buffer around each capital of 0.1 degree and the default SpatialPointsDataframe "capitals"
centroids <- cc_cen(centroid_capital, lon = "x_original", lat = "y_original", test = "country", verbose = TRUE) #country centroids are deleted again with the same defaults as centroid_capital

#Spatial resolution (error) ####

spatial_error <- subset(centroids, spatialError < 10000 | is.na(centroids$spatialError) == TRUE) #are we also removing the na's?? (Scott is going to ask Jens)

#Occurrences based on fossil material, germplasm, literature ####

clean_fosgermlit <- subset(spatial_error, !(issues == "gass84") & !(issues == "cdround"))
#discuss which ones we will be deleting, now I just 'tested' these ones

#Occurences located near sea/lakes ####

file <- raster("C:/Users/user/Documents/School/Thesis/Climatic data/bio_1.bil")
predictors <- stack(file)
plot(file)
plot(predictor)

#possible option when we are going to use multiple climatic layers
#files <- list.files(path= "C:/Users/user/Documents/School/Thesis/Climatic data/", pattern='\\.bil$') #import tif files- change the pattern to suit the type of file
#predictors <- stack(files) # 
#plot(files)

clean_fosgermlit$ID <- seq.int(nrow(clean_fosgermlit)) #format for biogeo
clean_fosgermlit %>% 
  keepmainfields(ID = 'ID', Species = 'Species', x = 'x_original', y = 'y_original') -> species_arrange_main_fields
species_nearest_cell <- nearestcell(dat = species_arrange_main_fields, rst = predictors) # returns a list
species_nearest_cell_dat <- species_nearest_cell$dat #select list

for (i in 1:nrow(species_nearest_cell_dat)) {
  if (is.na(species_nearest_cell_dat[i, 5]) == TRUE) { 
    species_nearest_cell_dat[i,5] <- species_nearest_cell_dat[i, 3]
  } else { 
    species_nearest_cell_dat[i,5] <-  species_nearest_cell_dat[i,5]
  } 
}

for (i in 1:nrow(species_nearest_cell_dat)) {
  if (is.na(species_nearest_cell_dat[i, 6]) == TRUE) { 
    species_nearest_cell_dat[i,6] <- species_nearest_cell_dat[i, 4]
  } else { 
    species_nearest_cell_dat[i,6] <-  species_nearest_cell_dat[i,6]
  } 
}

species_clean_nearestcell <- select(species_nearest_cell_dat, "Species", "longitude" = "x_original", "latitude" = "y_original")

#Second check for duplicates ####
#We check again for duplicates because the nearestcell function may have shifted some of the occurrences to the same location

clean_duplicates <- cc_dupl(species_clean_nearestcell, lon = "longitude", lat = "latitude", species = "Species", additions = NULL, verbose = TRUE)

clean_duplicates1 <- cc_equ(clean_duplicates, lon = "longitude", lat = "latitude", test = "identical", verbose = TRUE)

#Geographical outliers ####

#geo_out <- cc_outl(clean_duplicates1, lon = "longitude", lat = "latitude", species = "Species", value = "clean", verbose = TRUE) #searches geographical outliers by default

#Botanical garden + Hyper-anthropogenic environment

garden <- cc_inst(clean_duplicates1, lon = "longitude", lat = "latitude", verbose = TRUE)
#anthro <- cc_urb(garden, lon = "longitude", lat = "latitude", verbose = TRUE) no reference provided

#Clipping to Europe ####

ext <- extent(-10, 30, 36, 72) # this is the extent for Europe, but only a rough guess. Needs discussion
predictors <- crop(predictors, ext) # crop the extent to Europe
crs(predictors) <- "+proj=longlat +datum=WGS84 +no_defs" #set projection to WGS84
plot(predictors[[1]]) #have a look at the crop 
#[[1]] refers to the layer in the stack

setwd(choose.dir())
writeRaster(x = predictors, filename = "predictor_clipped", overwrite = TRUE, format = "GTiff", bylayer = TRUE)
setwd(choose.dir())

coordinates(garden) <- ~ longitude + latitude #convert to spatial object
cropped_occ <- crop(garden, predictors)

plot(predictors)
points(cropped_occ, col = "red", pch = 1, cex = 0.5)

#One point per grid cell ####
#We do this to reduce sampling bias (spatial), for example species closer to roads have a much higher change of being sampled causing an overestimation

species_1_each_grid <- gridSample(garden, predictors, n=1) #one point per grid cell
species_1_each_grid_df <- as.data.frame(species_1_each_grid) #convert to dataframe

plot(predictors)
points(species_1_each_grid, col = "red", pch = 1, cex = 0.5)

#########################################################################################################

identical(remnant_nearest_cell_ordered_rownames, remnant_grid_df) #check if identical
remnant_nearest_cell_ordered_rownames %>% rownames_to_column() -> remnant_nearest_cell_rownames # add rownames to the dataframe
merge(remnant_nearest_cell_rownames, remnant_grid_df, by = c('longitude', 'latitude')) %>% #merge on longitude and latitude
  select(location = rowname, longitude, latitude) -> remnant_biogeo_grid
dim(remnant_biogeo_grid)

#Thresholding ####
#Mapping ####
#install.packages("mapr")
library(mapr)

plot(cropped_occ)
plot(clean_duplicates1)
map_ggplot(cropped_occ, predictors)
?map_ggplot

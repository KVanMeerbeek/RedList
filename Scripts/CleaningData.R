#install.packages("CoordinateCleaner")
#install.packages("scrubr")
#install.packages("biogeo")
#install.packages("raster")
library(CoordinateCleaner)
library(scrubr)
library(biogeo) 
library(raster)

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

#Occurences located near sea/lakes ####

file <- raster("C:/Users/user/Documents/School/Thesis/Climatic data/bio_1.bil")
predictor <- stack(file)
plot(file)
plot(predictor)

#possible option when we are going to use multiple climatic layers
#files <- list.files(path= "C:/Users/user/Documents/School/Thesis/Climatic data/", pattern='\\.bil$') #import tif files- change the pattern to suit the type of file
#predictors <- stack(files) # 
#plot(files)

spatial_e <- spatial_error
names(spatial_e)

spatial_e$ID <- seq.int(nrow(spatial_e)) #format for biogeo
spatial_e %>% 
  keepmainfields(ID = 'ID', Species = 'Species', x = 'x_original', y = 'y_original') -> remnant_arrange_main_fields
remnant_nearest_cell <- nearestcell(dat = remnant_arrange_main_fields, rst = predictor) # returns a list
remnant_nearest_cell_dat <- remnant_nearest_cell$dat #select list

#Second check for duplicates ####
#We check again for duplicates because the nearestcell function may have shifted some of the occurrences to the same location

#Clipping to Europe ####

#Geographical outliers ####

#Botanical garden + Hyper-anthropogenic environment

#One point per grid cell ####
#We do this to reduce sampling bias (spatial), for example species closer to roads have a much higher change of being sampled causing an overestimation

#Thresholding ####
#Mapping ####
#install.packages("mapr")
library(mapr)

map_plot(clean_biological, size = 1, pch = 10) #----> weird: ask Scott!!
map_ggplot(clean3, map = "world")
setwd(choose.dir()) #set working directory (place where everything will be saved)

#install & load packages
#install.packages("spocc")
#install.packages("taxize")

library(spocc)

library(taxize)

#Taxize ####
#lookup all synonyms for a species name in the database of ITIS (must be length 1-vector, can also be other databases: tropicos, col, nbn, worms)
w <- c("Agrimonia eupatoria")
x <- synonyms(w, db = "itis")
x1 <- synonyms(w, db = "itis")
x2 <- synonyms(w, db = "itis")

a <- c("Aegopodium podagraria")
b <- synonyms(a, db = "itis")

procera <- c("Agrimonia procera")
procera_syn <- synonyms(procera, db = "itis")
#If the species has no synonyms an empty list will be returned. Then we just take the original input name (= w). When the species does has synonyms, we make a new vector (z = original name + synonym names). The outcome will be a character vector which can be used as an input for spocc
if (is.na(y) == TRUE) {
  z = w
} else {
  b <- x$`Gentiana pneumonanthe`$acc_name
  b1 <- x$`Gentiana pneumonanthe`$syn_name
  
  z <- c(b, b1)
}

#Spocc ####
#lookup observations of species in database
#limit is set to a very high number (100 000) so it returns all observations (by default it would be only 500)
species1 <- occ(query = z, from = c("gbif", "bison", "inat", "ecoengine", "idigbio", "ala"), limit = 100000)
species2 <- occ2df(species1) #extract key columns needed for making maps
#extract all columns of information instead of only the relevant
info1 <- species1$gbif$data$Gentiana_pneumonanthe
info2 <- species1$bison$data$Gentiana_pneumonanthe
info3 <- species1$inat$data$Gentiana_pneumonanthe
info4 <- species1$ebird$data$Gentiana_pneumonanthe
info5 <- species1$ecoengine$data$Gentiana_pneumonanthe
info6 <- species1$antweb$data$Gentiana_pneumonanthe
info7 <- species1$vertnet$data$Gentiana_pneumonanthe
info8 <- species1$idigbio$data$Gentiana_pneumonanthe
info9 <- species1$obis$data$Gentiana_pneumonanthe
info10 <- species1$ala$data$Gentiana_pneumonanthe

species3 <- rbind(info1, info2, info3, info4, info5, info6, info7, info8, info9, info10)
#BIEN ####
#install.packages("BIEN")
library(BIEN) #another type of database to get occurrences

speciesBIEN1 <- BIEN_occurrence_species(z) #again we take the same character vector from taxize
speciesBIEN2 <- data.frame(speciesBIEN1$scrubbed_species_binomial, speciesBIEN1$latitude, speciesBIEN1$longitude, speciesBIEN1$date_collected, speciesBIEN1$datasource, speciesBIEN1$datasource_id) #these are the interesting columns for map making

#combine both occurence data sets
#install.packages("plyr") #which contains the rename-function
library(plyr)

speciesBIEN3 <- rename(speciesBIEN2, c("speciesBIEN1.scrubbed_species_binomial" = "name", "speciesBIEN1.latitude" = "latitude", "speciesBIEN1.longitude" = "longitude", "speciesBIEN1.date_collected" = "date", "speciesBIEN1.datasource" = "prov", "speciesBIEN1.datasource_id" = "key")) #renaming the columns so it coincides with the columnnames of data frame "species"

data <- rbind(species2, speciesBIEN3) #this merges the two data frames of occurences vertically to each other
data$longitude <- as.numeric(as.character(data$longitude)) #both latitude and longitude where orignially character
data$latitude <- as.numeric(as.character(data$latitude))

#Data cleaning ####

#install.packages("CoordinateCleaner")
library(CoordinateCleaner)
#install.packages("scrubr")
library(scrubr)

clean_duplicates <- cc_dupl(data, lon = "longitude", lat = "latitude", species = "name", additions = NULL, verbose = TRUE)
clean_duplicates1 <- cc_equ(clean_duplicates, lon = "longitude", lat = "latitude", test = "identical", verbose = TRUE) #additional test to be sure that all duplicates are really gone. Additionally checks on identical coordinates.
clean_incomplete <- coord_incomplete(clean_duplicates1, lat = "latitude", lon = "longitude", drop = TRUE) #remove incomplete coordinates (removal of rows which only consist of NA's)
clean_impossible <- coord_impossible(clean_incomplete, lat = "latitude", lon = "longitude", drop = TRUE) #remove impossible coordinates #will normally give the same result after the cleaning steps which have already be done, just a built in security
clean_unlikely <- coord_unlikely(clean_impossible, lat = "latitude", lon = "longitude", drop = TRUE) #remove unlikely coordinates (eg 0,0) which are indicators for erroneous data and high concentrations around equator and prime meridian (Greenwich) 

clean_biological <- CleanCoordinates(clean_impossible, lon = "longitude", lat = "latitude", species = "name") #test if all records are valid (if validity = TRUE --> OK!)

#install.packages("biogeo")
library(biogeo) #we need this to relocate occurrences in the sea to the nearest terrestrial cell

names(clean_unlikely) <- c("Species", "x_original", "y_original", "prov", "date", "ID")
biogeo1 <- addmainfields(clean_unlikely, "Species")


#Mapping ####
#install.packages("mapr")
library(mapr)

map_plot(clean_biological, size = 1, pch = 10) #----> weird: ask Scott!!
map_ggplot(clean3, map = "world")

#MaxEnt ####

#packages needed for Maxent
#install.packages("raster")
library(raster)
#install.packages("dismo")
library(dismo)
#install.packages("rgeos")
library(rgeos)
#install.packages("rJava")
library(rJava)

#settings for the R-markdown document
#install.packages("knitr")
library(knitr)

knitr::opts_knit$set(root.dir = 'C:/Users/user/Documents/School/Thesis/Script/Maxent')
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

#set up the Maxent path
utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.3.3k/maxent.jar", 
                     destfile = paste0(system.file("java", package = "dismo"), 
                                       "/maxent.jar"), mode = "wb") #wb for binary file, otherwise maxent.jar can not execute

#prepare data inputs
if (!file.exists("C:/Users/user/Documents/School/Thesis/Script/Maxent/data")) dir.create("C:/Users/user/Documents/School/Thesis/Script/Maxent/data")
if (!file.exists("C:/Users/user/Documents/School/Thesis/Script/Maxent/data/bioclim")) dir.create("C:/Users/user/Documents/School/Thesis/Script/Maxent/data/bioclim")
if (!file.exists("C:/Users/user/Documents/School/Thesis/Script/Maxent/data/studyarea")) dir.create("C:/Users/user/Documents/School/Thesis/Script/Maxent/data/studyarea")
if (!file.exists("C:/Users/user/Documents/School/Thesis/Script/Maxent/output")) dir.create("C:/Users/user/Documents/School/Thesis/Script/Maxent/output")
require(utils)

#download climatic data
utils::download.file(url = "http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio_10m_bil.zip", 
                     destfile = paste0("C:/Users/user/Documents/School/Thesis/Script/Maxent/data/bioclim/bio_10m_bil.zip"))
utils::unzip("C:/Users/user/Documents/School/Thesis/Script/Maxent/data/bioclim/bio_10m_bil.zip", exdir = "C:/Users/user/Documents/School/Thesis/Script/Maxent/data/bioclim/bio_10m_bil")

clim_list <- list.files("C:/Users/user/Documents/School/Thesis/Script/Maxent/data/bioclim/bio_10m_bil", pattern = ".bil$", full.names = T)
clim <- raster::stack(clim_list)
crs(clean_unlikely) <- crs(clim) #make sure our occurrences and climatic data are defined in the same crs

#occurence data
coordinates(clean_unlikely) <- ~longitude+latitude #spatial relationship with environmental layers
plot(clim[[1]]) 
plot(clean_unlikely, add = TRUE)

#remove erroneous points (i.e., only keep good records) be using a logical expression and boolean operators (LATER ON, HAS TO BE DONE)

#thin occ data (keep one occurrence point per cell) (ALSO STILL HAS TO BE DONE)

#Set up study area 
#By setting a buffer arround the occurrences, we define the study area
occ_buff <- buffer(clean_unlikely, width = 10, dissolve = TRUE) #width = define how many meters we want to buffer around the points (in meters)
plot(clim[[1]])

plot(clean_unlikely, add = T, col = "red")  # adds occurrence data to the plot
plot(occ_buff, add = T, col = "blue")  # adds buffer polygon to the plot


plot(clim[[1]])

plot(clean_unlikely, add = T, col = "red")  # adds occurrence data to the plot
plot(occ_buff, add = T, col = "blue")  # adds buffer polygon to the plot --> some fault in it due to crs...

#clipping the study area
studyArea <- crop(clim,extent(occ_buff))  
studyArea <- mask(studyArea,occ_buff)

writeRaster(studyArea,
            # a series of names for output files
            filename=paste0("C:/Users/user/Documents/School/Thesis/Script/Maxent/data/studyarea/", names(studyArea),".asc"), 
            format="ascii", ## the output format
            bylayer=TRUE, ## this will save a series of layers
            overwrite=T)

#select 10000 random background points from the study area. To make our experiment reproducible (i.e. select the same set of points), we used a static seed via set.seed(1) function. Then, we plotted the background points together with the study area and occurrence data
set.seed(1) 
bg <- sampleRandom(x = studyArea,
                   size = 10000,
                   na.rm = T, #removes the 'Not Applicable' points  
                   sp = T) # return spatial points 

plot(studyArea[[1]])


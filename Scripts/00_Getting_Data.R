setwd(choose.dir()) #set working directory (place where everything will be saved)

#install.packages("taxize")
#install.packages("spocc")
#install.packages("dplyr")
#install.packages("BIEN")
library(taxize)
library(spocc)
library(dplyr)
library(BIEN)

species_list <- read.csv("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/DataLoopTest.csv", header = TRUE, sep = ";")
colnames(species_list) <- c("Scientific Name", "Dutch Name", "Red List Category")
species_list[, ] <- lapply(species_list[, ], as.character) #columns need to be character to be later used in occ-function of spocc; lapply loops over each columns while using as.character on each one


#Taxize ####
#lookup all synonyms for a species name in the database of ITIS (must be length 1-vector, can also be other databases: tropicos, col, nbn, worms)

for (i in 1:nrow(species_list)) {
  syn <- synonyms(species_list[i, 1], db = "itis")
  
  x <- syn[[species_list[i, 1]]] #this should be reading the species name and not just "species_list[i, 1]"
  
  if (is.na(x) == TRUE) { #if there is no data (TSN) for the species in the database, there wil be a NA
    y = c(species_list[i, 1])
  } else if (ncol(x) == 3) { #when there is data in the database, but no synonyms we get this
    y = c(species_list[i, 1])
  } else { #when there is data and synonyms we have to
    z <- species_list[i, 1]
    z1 <- x$syn_name
    
    y <- c(z, z1)
  } #for-loop is still open en we continue with this vector of synonyms as an input for spocc
  
  #write.csv(y ,paste("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/Synonyms_taxize/", species_list[i, 1],".csv", sep="")) #maybe still need this

#Spocc ####
#lookup observations of species in database
#limit is set to a very high number (1 000 000 for gbif, the rest on 100000) so it returns all observations (by default it would be only 500) 
  species <- occ(query = y, from = c("gbif", "bison", "inat", "ecoengine", "idigbio", "ala"), gbifopts = list(limit = 1000000), bisonopts = list(limit = 100000), inatopts = list(limit = 100000), ecoengineopts = list(limit = 100000), idigbioopts = list(limit = 100000), alaopts = list(limit = 100000))
  
#we need to clean every data.frame in the list separatly for gbif, bison... because they differ in column numbers and column names

  gbif <- species$gbif$data #should become [[species[i, 1]]] for one species it is Agrostemma_githago
  for (j in 1:length(gbif)) {
  if (length(gbif[[j]]) == 0) {
    gbif1 <- data.frame()
  } else {
    gbif1 <- select(gbif[[j]], "databaseName" = "name", "x_original" = "longitude", "y_original" = "latitude", "prov" = "prov", "spatialError" = "coordinateUncertaintyInMeters", "issues" = "issues")
    gbif1 <- data.frame(y[[1]], gbif1)
    gbif1 <- select(gbif1, "Species" = "y..1..", "databaseName", "x_original", "y_original", "prov", "spatialError", "issues") }
    if (j == 1) {
      data_gbif <- gbif1
    } else {
      data_gbif <- rbind(data_gbif, gbif1)
    }
  }
  
  bison <- species$bison$data
  for (k in 1:length(bison)) {
  if (length(bison[[k]]) == 0) {
    bison1 <- data.frame()  
  } else {
    bison1 <- select(bison[[k]], "databaseName" = "name", "x_original" = "longitude", "y_original" = "latitude", "prov" = "prov", "spatialError" = "coordinateUncertaintyInMetrs")
    bison1 <- data.frame(y[[1]], bison1, "")
    bison1 <- select(bison1, "Species" = "y..1..", "databaseName", "x_original", "y_original", "prov", "spatialError", "issues" = "X..")
  }
    if (k == 1) {
      data_bison <- bison1
    } else {
      data_bison <- rbind(data_bison, bison1)
    } #still need some check up for the column names of bison
  }
  
  inat <- species$inat$data
  for (l in 1:length(inat)) {
  if (length(inat[[l]]) == 0) {
    inat1 <- data.frame()   
  } else {
    inat1 <- select(inat[[l]], "databaseName" = "name", "x_original" = "longitude", "y_original" = "latitude", "prov" = "prov", "spatialError" = "positional_accuracy")
    inat1 <- data.frame(y[[1]], inat1, "")
    inat1 <- select(inat1, "Species" = "y..1..", "databaseName", "x_original", "y_original", "prov", "spatialError", "issues" = "X..")
  }
   if (l == 1) {
     data_inat <- inat1
   } else {
     data_inat <- rbind(data_inat, inat1)
   }
  }
  
  ecoengine <- species$ecoengine$data
  for (m in 1:length(ecoengine)) {
  if (length(ecoengine[[m]]) == 0) {
    ecoengine1 <- data.frame()  
  } else {
    ecoengine1 <- select(ecoengine[[m]], "databaseName" = "name", "x_original" = "longitude", "y_original" = "latitude", "prov" = "prov", "spatialError" = "coordinate_uncertainty_in_meters")
    ecoengine1 <- data.frame(y[[1]], ecoengine1, "")
    ecoengine1 <- select(ecoengine1, "Species" = "y..1..", "databaseName", "x_original", "y_original", "prov", "spatialError", "issues" = "X..")
  }
   if (m == 1) {
    data_ecoengine <- ecoengine1
   } else {
    data_ecoengine <- rbind(data_ecoengine, ecoengine1)
   }
  }
  
  idigbio <- species$idigbio$data
  for (n in 1:length(idigbio)) {
  if (length(idigbio[[n]]) == 0) {
    idigbio1 <- data.frame()   
  } else {
    idigbio1 <- select(idigbio[[n]], "databaseName" = "name", "x_original" = "longitude", "y_original" = "latitude", "prov" = "prov", "spatialError" = "coordinateuncertainty")
    idigbio1 <- data.frame(y[[1]], idigbio1, "")
    idigbio1 <- select(idigbio1, "Species" = "y..1..", "databaseName", "x_original", "y_original", "prov", "spatialError", "issues" = "X..")
  }
   if (n == 1) {
    data_idigbio <- idigbio1
   } else {
      data_idigbio <- rbind(data_idigbio, idigbio1)
   }
  }
  
#BIEN ####
  speciesBIEN <- BIEN_occurrence_species(y) #again we take the same character vector from taxize
  if (length(speciesBIEN) == 0) {
    speciesBIEN1 <- data.frame()   
  } else {
    speciesBIEN1 <- select(speciesBIEN, "databaseName" = "scrubbed_species_binomial", "x_original" = "longitude", "y_original" = "latitude", "prov" = "datasource")
    speciesBIEN1 <- data.frame(y[[1]], speciesBIEN1, "", "")
    speciesBIEN1 <- select(speciesBIEN1, "Species" = "y..1..", "databaseName", "x_original", "y_original", "prov", "issues" = "X..", "spatialError" = "X...1")
  }
  
  data <- rbind(data_gbif, data_bison, data_inat, data_ecoengine, data_idigbio, speciesBIEN1)
  data$x_original <- as.numeric(data$x_original) #otherwise we get errors when cleaning the data
  data$y_original <- as.numeric(data$y_original)
  data$spatialError <- as.numeric(data$spatialError)
  
  write.csv(data, paste("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/Occurrence_data_spocc/", species_list[i, 1],".csv", sep="")) 
  
} #here we will finally close the for-loop eventually

#SideNotes ####
  
  
  #Ala database can be dropped because it wil only give results in like 1% of the cases....
  #ala <- species$ala$data$Agrostemma_githago #should become [[species[i, 1]]]
  #ala1 <- select(ala, "databaseName" = "name", "x_original" = "longitude", "y_original" = "latitude", "prov" = "prov", "spatialError" = "coordinateuncertainty")
  #ala1 <- data.frame("Agrostemma githago", ala1, "")
  #ala1 <- select(ala1, "species" = "X.Agrostemma.githago.", "databaseName", "x_original", "y_original", "prov", "spatialError", "issues" = "X..")
  
  #problem when looping
  #each synonym is in a separate list, need to find a way to get these out and put them underneath eachother





setwd(choose.dir()) #set working directory (place where everything will be saved)

#install.packages("taxize")
#install.packages("spocc")
#install.packages("dplyr")
library(taxize)
library(spocc)
library(dplyr)

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
  
  write.csv(y ,paste("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/Synonyms_taxize/", species_list[i, 1],".csv", sep="")) #maybe still need this

#Spocc ####
#lookup observations of species in database
#limit is set to a very high number (1 000 000 for gbif, the rest on 100000) so it returns all observations (by default it would be only 500) 
  species <- occ(query = y, from = c("gbif", "bison", "inat", "ecoengine", "idigbio", "ala"), gbifopts = list(limit = 1000000), bisonopts = list(limit = 100000), inatopts = list(limit = 100000), ecoengineopts = list(limit = 100000), idigbioopts = list(limit = 100000), alaopts = list(limit = 100000))
  
#we need to clean every data.frame in the list separatly for gbif, bison... because they differ in column numbers and column names

  gbif <- species$gbif$data$Agrostemma_githago #should become [[species[i, 1]]]
  gbif1 <- data.frame("Agrostemma githago", gbif$name, gbif$longitude, gbif$latitude, gbif$prov, gbif$issues, gbif$coordinateUncertaintyInMeters) 
  colnames(gbif1) <- c("species", "database name", "x_original", "y_original", "prov", "issues", "spatial error")

  bison <- species$bison$data$Agrostemma_githago #should become [[species[i, 1]]]
  bison1 <- data.frame("Agrostemma githago", gbif$name, gbif$longitude, gbif$latitude, gbif$prov, gbif$issues, gbif$coordinateUncertaintyInMeters) 
  colnames(gbif1) <- c("species", "database name", "x_original", "y_original", "prov", "issues", "spatial error")
  
  inat <- species$inat$data$Agrostemma_githago #should become [[species[i, 1]]]
  inat1 <- data.frame("Agrostemma githago", inat$name, inat$longitude, inat$latitude, inat$prov, "", inat$positional_accuracy) 
  colnames(inat1) <- c("species", "database name", "x_original", "y_original", "prov", "issues", "spatial error")
  
  ecoengine <- species$ecoengine$data$Agrostemma_githago #should become [[species[i, 1]]]
  ecoengine1 <- data.frame("Agrostemma githago", ecoengine$name, ecoengine$longitude, ecoengine$latitude, ecoengine$prov, "", ecoengine$coordinate_uncertainty_in_meters) 
  colnames(ecoengine1) <- c("species", "database name", "x_original", "y_original", "prov", "issues", "spatial error")
  
  data <- rbind(gbif1, inat1, ecoengine1) 
  
  write.csv(species1, paste("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/Occurrence_data_spocc/", species_list[i, 1],".csv", sep=""))
} #here we finally close the for-loop 


  
 # write.csv(y ,paste("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/Synonyms_taxize/", species_list[i, 1],".csv", sep="")) #maybe still need this

#species1 <- occ2df(species) #this one needs to be replaced by a suitable data frame





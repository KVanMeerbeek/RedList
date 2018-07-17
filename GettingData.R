setwd(choose.dir()) #set working directory (place where everything will be saved)

#install.packages("spocc")
library(spocc)
#install.packages("taxize")
library(taxize)

species_list <- read.csv("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/DataLoopTest.csv", header = TRUE, sep = ";")
colnames(species_list) <- c("Scientific Name", "Dutch Name", "Red List Category")
species_list[, 1] <- as.character(species_list[, 1])
species_list[, 2] <- as.character(species_list[, 2])
species_list[, 3] <- as.character(species_list[, 3])

#Taxize ####
#lookup all synonyms for a species name in the database of ITIS (must be length 1-vector, can also be other databases: tropicos, col, nbn, worms)

for (i in 1:nrow(species_list)) {
  syn <- synonyms(species_list[i, 1], db = "itis")
  syn <- c(species_list[i,1], syn$`species_list[i,1]`$syn_name)
  
  write.csv(syn ,paste("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/Synonyms_taxize/", species_list[i, 1],".csv", sep=""))
}

#Spocc ####
#lookup observations of species in database
#limit is set to a very high number (1 000 000 for gbif, the rest on 100000) so it returns all observations (by default it would be only 500)

for (i in 1:nrow(species_list)) {
  species <- occ(query = species_list[i, 1], from = c("gbif", "bison", "inat", "ecoengine", "idigbio", "ala"), gbifopts = list(limit = 1000000), bisonopts = list(limit = 100000), inatopts = list(limit = 100000), ecoengineopts = list(limit = 100000), idigbioopts = list(limit = 100000), alaopts = list(limit = 100000))
  species1 <- occ2df(species)
  write.csv(species1, paste("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/Occurrence_data_spocc/", species_list[i, 1],".csv", sep=""))
}

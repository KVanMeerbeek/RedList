setwd(choose.dir()) #set working directory (place where everything will be saved)

#install.packages("spocc")
library(spocc)
#install.packages("taxize")
library(taxize)

species_list <- read.csv("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/DataLoopTest.csv", header = TRUE, sep = ";")
colnames(species_list) <- c("Scientific Name", "Dutch Name", "Red List Category")

#Taxize ####
#lookup all synonyms for a species name in the database of ITIS (must be length 1-vector, can also be other databases: tropicos, col, nbn, worms)

for (i in 1:nrow(species_list)) {
  syn <- synonyms(species_list[i, 1], db = "itis")
  syn <- c(species_list[i,1], syn$`species_list[i,1]`$syn_name)
  write.csv(syn ,paste("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/Synonyms_taxize/", species_list[i, 1],".csv", sep=""))
}

#Spocc ####
#lookup observations of species in database
#limit is set to a very high number (100 000) so it returns all observations (by default it would be only 500)

for (i in 1:nrow(species_list)) {
  species <- occ(query = species_list[i, 1], from = c("gbif", "bison", "inat", "ecoengine", "idigbio", "ala"), limit = 100000)
  usewrite.csv(species ,paste("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/Occurrence_data_spocc/", species_list[i, 1],".csv", sep=""))
}

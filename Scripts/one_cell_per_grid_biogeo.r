#Tuatara ENM
#Author Scott Jarvie

#load libraries ---
install.packages("pacman", repos="https://cloud.r-project.org") #only need to do once
pacman::p_load(biogeo, 
               spThin,
               raster,
               dismo,
               tidyverse, update = F)

# import climate data -----
# set working directory
setwd("") # set working directory
files <- list.files(pattern='\\.tif$') #import tif files; I use tiffs but put whatever pattern makes sense
predictors <- stack(files)
ext <- extent(166,179,-48,-33.5) # crop the extent to NZ; modify for Europe
predictors <- crop(predictors, ext) # crop the extent 
#crs(predictors) <- "+proj=longlat +datum=WGS84 +no_defs" # set projection if necessary
#maybe you want to reproject- will have to use another function





#import occurence data for remnant populations ----
#setwd("~/Desktop/")
setwd("C:/Data/nz/tuatara_correlative_SDM/csv/") # set working directory
remnant <- read.csv("remnant.csv") # import remnant population csv
head(remnant); str(remnant) # examine data
remnant %>% 
  arrange(desc(latitude)) %>% # arange in descending order for latitude
  select(location, latitude, longitude, species = type) -> remnant_arrange 

# nearestcell ----
remnant_arrange$ID <- seq.int(nrow(remnant_arrange)) #format for biogeo
remnant_arrange %>% 
  keepmainfields(ID = 'ID', Species = 'species', x = 'longitude', y = 'latitude') -> remnant_arrange_main_fields
remnant_arrange_main_fields_filter <- remnant_arrange_main_fields %>% filter(Species == "remnant") # keep data for only remnant populations
remnant_nearest_cell <- nearestcell(remnant_arrange_main_fields_filter, predictors) # returns a list
remnant_nearest_cell_dat <- remnant_nearest_cell$dat #select list

remnant_nearest_cell_dat %>% 
  select(longitude = x, latitude = y) %>% 
  arrange(desc(latitude)) %>%
  bind_cols(remnant, remnant_nearest_cell_dat) %>%
  select(location, longitude = longitude1, latitude = latitude1) %>%
  column_to_rownames(var = 'location') -> remnant_nearest_cell_ordered_rownames

#remant one point per grid cell----
remnant_not_duplicate <- duplicated(remnant_nearest_cell_ordered_rownames[, c('longitude', 'latitude')]) #check if values are duplicated
remnant_not_duplicated <- remnant_nearest_cell_ordered_rownames[!remnant_not_duplicate, ] #remove duplicated values
remnant_not_duplicate_sp <- remnant_not_duplicated
coordinates(remnant_not_duplicate_sp) <- ~ longitude + latitude #convert to sp object
remnant_not_duplicate_grid <- gridSample(remnant_not_duplicate_sp, predictors[[1]], n=1) #one point per grid cell
remnant_grid_df <- as.data.frame(remnant_not_duplicate_grid) #convert to dataframe

identical(remnant_nearest_cell_ordered_rownames, remnant_grid_df) #check if identical
remnant_nearest_cell_ordered_rownames %>% rownames_to_column() -> remnant_nearest_cell_rownames # add rownames to the dataframe
merge(remnant_nearest_cell_rownames, remnant_grid_df, by = c('longitude', 'latitude')) %>% #merge on longitude and latitude
  select(location = rowname, longitude, latitude) -> remnant_biogeo_grid
dim(remnant_biogeo_grid)

#remnant spThin 1km ----
remnant_1km_spThin <- thin.algorithm(remnant_nearest_cell_ordered_rownames, thin.par = 1, reps = 100)
summaryThin(remnant_1km_spThin)#
remnant_1km_spThin_1 <- remnant_1km_spThin[[100]] #select the 100 repitition - can you other
remnant_1km_spThin_1$type <- "remnant_1km" #add a column
rownames_to_column(remnant_1km_spThin_1, "location") %>% 
  select(location, type, longitude = Longitude, latitude = Latitude) -> remnant_1km_biogeo_spThin

#remnant spThin 5km - try for a different spatial thinning resolution----
remnant_5km_spThin <- thin.algorithm(remnant_nearest_cell_ordered_rownames, thin.par = 5, reps = 100)
summaryThin(remnant_5km_spThin)#
remnant_5km_spThin_1 <- remnant_5km_spThin[[100]]
remnant_5km_spThin_1$type <- "remnant_5km"
rownames_to_column(remnant_5km_spThin_1, "location") %>% 
  select(location, type, longitude = Longitude, latitude = Latitude) -> remnant_5km_biogeo_spThin





#write_csv ----
write_csv(x = remnant_biogeo_grid, path = "C:/Users/au596783/Desktop/remnant_biogeo_grid.csv", append = FALSE)

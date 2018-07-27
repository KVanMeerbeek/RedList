setwd("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/CleanData")
files_cleanData <- list.files(pattern = "\\.csv$")

for (w in 1:length(files_cleanData)) {
spec <- read.csv(files_cleanData[[w]], header = TRUE, sep = ",")
spec1 <- spec[, 2:4]
coordinates(spec1) <- ~ longitude + latitude

spec_buffer <- buffer(spec1, 50000, dissolve = TRUE)

plot(predictor_19[[1]])
points(spec1, col = "red", pch = 1, cex = 0.5)
plot(spec_buffer, add = TRUE, col = "blue")

background <- crop(predictor_19[[1]], extent(spec_buffer))
background <- mask(background, spec_buffer)

set.seed(1)
bg <- sampleRandom(x = background, size = 10000, na.rm = TRUE, sp = TRUE)

plot(predictor_19[[1]])
plot(bg, add = TRUE, col = "blue", pch = 3, cex = 0.5)
plot(spec_buffer, add = TRUE, col = "red")

write.csv(bg, paste("C:/Users/user/Documents/School/Thesis/Data RedListSpecies/BackgroundPointsPerSpecies/", spec[1,2],".csv", sep=""))
}

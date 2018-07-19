#install.packages("rgbif")
#install.packages("xlsx")
library(rgbif)
library(xlsx)

issues <- gbif_issues()
write.xlsx(issues, "C:/Users/user/Documents/School/Thesis/Script/issues/issues.xlsx")

library(dplyr)
library(usmap)
library(stringr)
library(lubridate)
# # FEMA data link: https://www.fema.gov/openfema-data-page/public-assistance-funded-projects-details-v1
# data <- read.csv('PublicAssistanceFundedProjectsDetails.csv', head = TRUE)
 
# # orginal data is too large to upload, select hurricane and stoems from 2009 - 2018 and save as new data file.
# hurricane <- data %>% filter(incidentType %in% c("Hurricane","Severe Storm(s)"))
# hurricane <- hurricane %>% select(-c("hash","lastRefresh","id"))
# hurricane$declarationDate <-  as_datetime(hurricane$declarationDate)
# hurricane$declarationYear <-  year(hurricane$declarationDate)
# hurricane <- hurricane %>% filter(declarationYear > 2009 & declarationYear < 2018)
# write.csv(hurricane,"hurricane.csv",row.names=FALSE)

# Read data as hrc
hrc <- read.csv("hurricane.csv")

# get fips
hrc$Fips <- NA
for (i in 1:dim(hrc)[1]){
  if(hrc$countyCode[i] > 99){
    if(hrc$stateNumberCode[i] > 9){
      hrc$Fips[i] <- str_c(as.character(hrc$stateNumberCode[i]),as.character(hrc$countyCode[i]))
    }
    else{
      hrc$Fips[i] <- str_c("0", as.character(hrc$stateNumberCode[i]),as.character(hrc$countyCode[i]))
    }
  }
  else if(hrc$countyCode[i] > 9){
    if(hrc$stateNumberCode[i] > 9){
      hrc$Fips[i] <- str_c(as.character(hrc$stateNumberCode[i]), "0", as.character(hrc$countyCode[i]))
    }
    else{
      hrc$Fips[i] <- str_c("0", as.character(hrc$stateNumberCode[i]), "0", as.character(hrc$countyCode[i]))
    }
  }
  else if(hrc$countyCode[i] != 0){
    if(hrc$stateNumberCode[i] > 9){
      hrc$Fips[i] <- str_c(as.character(hrc$stateNumberCode[i]), "00", as.character(hrc$countyCode[i]))
    }
    else{
      hrc$Fips[i] <- str_c("0", as.character(hrc$stateNumberCode[i]), "00", as.character(hrc$countyCode[i]))
    }
  }
}

# select useful columns for mapping
colnames(hrc)

# # check the information in dcc-damagecategory and damageCategoryCode, reserve damageCategoryCode.
# unique(paste(hrc$dcc,hrc$damageCategory,sep = " - "))
# unique(hrc$damageCategoryCode)

hrc <- hrc %>% select("disasterNumber", "declarationYear", "incidentType","applicationTitle","damageCategoryCode","projectSize","stateNumberCode","projectAmount", "federalShareObligated", "totalObligated","Fips","obligatedDate")

write.csv(hrc,"hrc.csv",row.names=FALSE)


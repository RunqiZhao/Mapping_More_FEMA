library(dplyr)
library(usmap)
library(stringr)
library(lubridate)
# data <- read.csv('PublicAssistanceFundedProjectsDetails.csv', head = TRUE)
# hurricaneF <- data %>% filter(disasterNumber %in% c("1292","1293","1294","1295","1296","1297","1299","1300","1302","1303","1305","1307","1308","3143","3144","3145","3146","3147","3148","3149"))
# write.csv(hurricaneF,"hurricaneF.csv")

hurricaneF <- read.csv("hurricaneF.csv")
# hurricaneF %>% 
#   group_by(countyCode) %>%
#   summarize(n())


# get fips
hurricaneF$Fips <- NA
for (i in 1:dim(hurricaneF)[1]){
  if(hurricaneF$countyCode[i] > 99){
    hurricaneF$Fips[i] <- str_c(as.character(hurricaneF$stateNumberCode[i]),as.character(hurricaneF$countyCode[i]))
  }
  else if(hurricaneF$countyCode[i] > 10){
    hurricaneF$Fips[i] <- str_c(as.character(hurricaneF$stateNumberCode[i]),"0",as.character(hurricaneF$countyCode[i]))
  }
}

# data-time
hurricaneF <- hurricaneF %>% 
  select("disasterNumber","declarationDate","incidentType","applicantId","dcc", "damageCategory","projectSize","projectAmount", "federalShareObligated","totalObligated", "obligatedDate","Fips")
hurricaneF$declarationDate <-  as_datetime(hurricaneF$declarationDate)
hurricaneF$obligatedDate <-  as_datetime(hurricaneF$obligatedDate)

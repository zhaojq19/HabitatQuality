library(xlsx)
library(magrittr)
library(dplyr)

table_all <- readRDS("E:/PAHQ/Data/1_YearTable_AllContinent.rds") 

table_absent <- subset(table_all,STATUS_YR==0)
table_yr <- subset(table_all,STATUS_YR>0)
count_yr <- table_yr %>% count(ISO3)
#InISO3: bool
#F, calculate mean from all PAs; 
#T,calculate mean from PAs located in the same country
count_yr$InISO3 <- ifelse(count_yr$n<5,F,T)

table_absent$InISO3 <- count_yr$InISO3[match(table_absent$ISO3,count_yr$ISO3)]
table_absent$InISO3 <- ifelse(is.na(table_absent$InISO3),F,table_absent$InISO3)
table_absample <- table_absent[,c(2,5,6,7)]
table_absample$median <- NA
table_absample$SD <- NA
for(i in 1:length(table_absample$WDPA_PID))
{
  if(table_absample[i,]$InISO3==T) {
    t_from <- subset(table_yr,ISO3==table_absample[i,]$ISO3)
  } else {
    t_from <- table_yr
  }
  table_absample[i,]$median <- floor(median(t_from$STATUS_YR))
}

table_final <- table_all[,c(2,3,5,6)]
table_final$New_Year <- ifelse(table_final$STATUS_YR==0,
                               table_absample$median[match(table_final$WDPA_PID,table_absample$WDPA_PID)],
                               table_final$STATUS_YR)
saveRDS(table_final, "E:/PAHQ/ProcessedData/1_YearTable_FinalNewYear.rds")



#####读入包######
library(xlsx)
library(ggplot2)
library(dplyr)
library(lemon)#reposition_legend
library(reshape2)#melt
library(deeptime)#ggarrange2

dataDir <- "E:/PAHQ/Data/"
outDir <- "E:/PAHQ/ProcessedData/"
##### read data ######
name_th <- c("Urban","NonIrriCrp","IrriCrp","CrpVeg","VegCrp")
name_con <- c("Africa","Asia","Europe",
              "North America","Oceania","South America")

# information on PAs in Chinese mainland
t.chn <- read.xlsx("E:/PAHQ/Data/3_AttrTable_Chinese_mainland.xls",1,encoding = "UTF-8")
t.chn$IUCN_int <- NA
t.chn$IUCN_int <- ifelse(t.chn$Level_en %in% c("国家级", "省级"),1,NA)


#####Analyze HD caused by each threat#####
for(i in 0:6)
{
  for(th_i in 1:5)
  {
    HD_slope <- readRDS(paste0(dataDir,"HD_",name_th[th_i],".rds"))
    HD_slope <- subset(HD_slope, NewYR_int <= 1992)
    HD_slope <- subset(HD_slope, COUNT > 5)
    #####cope with Chinese PAs#####
    HD_slope$IUCN_int <- ifelse(HD_slope$ISO3=="CHN",
                                t.chn$IUCN_int[match(HD_slope$WDPA_PID,t.chn$序号)],
                                HD_slope$IUCN_int)
    HD_slope <- subset(HD_slope, ISO3!="CHN" | ( ISO3=="CHN" & !is.na(IUCN_int)))
    HD_slope$CONTINENT <- ifelse(HD_slope$ISO3=="CHN","Asia",HD_slope$CONTINENT)
    #####Continent#####
    HD_slope$IUCN_int <- ifelse(HD_slope$IUCN_int<=2,1,HD_slope$IUCN_int-1)
    if(i==0)
    {
      HD_slope <- HD_slope
    } else{
      HD_slope <- subset(HD_slope,CONTINENT==name_con[i])
    }
    #####Statistical analysis#####
    HD_slope <- HD_slope[,which(colnames(HD_slope) %in% c("WDPA_PID","IUCN_int","CONTINENT",
                                                          paste0("hd_",as.character(1992:2020)) ))]
    HD_slopelong <- melt(HD_slope, id.vars = c("WDPA_PID","IUCN_int","CONTINENT"),
                         measure.vars = paste0("hd_",as.character(1992:2020)),
                         variable.name = c('Year'),#聚合变量的新列名
                         value.name = 'HD')#聚合值的新列名
    HD_table <- HD_slopelong %>% 
      group_by(Year) %>%
      summarise(meanHD = mean(HD,na.rm=T),
                stdHD = sd(HD,na.rm=T))
    HD_table <- na.omit(HD_table)
    HD_table <- as.data.frame(HD_table)
    HD_table$Year <- gsub("hd_","",HD_table$Year)
    HD_table$Year <- as.numeric(as.character(HD_table$Year))
    HD_table$Threat <- name_th[th_i]
    
    if(th_i==1){
      final_table1 <- HD_table
    } else{
      final_table1 <- rbind(final_table1,HD_table)
    }
  }
  final_table1$NewClass <- i
  if(i==0)
  {
    final_table2 <- final_table1
  } else{
    final_table2 <- rbind(final_table2,final_table1)
  }
}
final_table2$NewClass <- as.factor(final_table2$NewClass)
final_table2$NewClass <- recode_factor(final_table2$NewClass,
                                       "0"='All',
                                       "1"='Africa',
                                       "2"="Asia",
                                       "3"='Europe',
                                       "4"='North America',
                                       "5"='Oceania',
                                       "6"='South America')
saveRDS(object = final_table2,
        file = paste0(outDir,"3_HD_EachThreat.rds"))

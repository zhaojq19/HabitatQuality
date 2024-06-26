##### Setting #####

library(lme4)
library(reshape2)
library(dplyr)

outDir <- "E:/PAHQ/ProcessedData/"
data_forHD <- readRDS("E:/PAHQ/Data/4_Data_BuildModel.rds")

##### Standardize #####

colnames(data_forHD)
t.notsd <- c("HD_slope","IUCN_int","ISO3_update",
             "BIOME_NAME","WDPA_PID","PerDisArea")
t.sd <- c("LogAge","LogArea","LogElev", "RamanSuit",
          "LogPopDens", "LogGDP","GINI", "LogFert")
data_forHD_notsd <- data_forHD[,t.notsd]
data_forHD_sd <- data_forHD[,t.sd]
data_forHD_sd <- as.data.frame(apply(X = data_forHD_sd, MARGIN = 2,
                                     FUN = function(x){(x - mean(x,na.rm=T)) / sd(x,na.rm=T)}))
data_forHD <- cbind(data_forHD_notsd,data_forHD_sd)
data_forHD$ISO3_update <- droplevels(data_forHD$ISO3_update)
colnames(data_forHD)


##### Build Model #####

model_HDslope <- lmer(data_forHD~IUCN_int+poly(LogArea,2)+poly(LogElev,2)+
                        poly(LogPopDens,2)+IUCN_int:poly(LogAge,2)+
                        IUCN_int:poly(LogGDP,2)+IUCN_int:poly(GINI,2)+
                        poly(LogAge,2):poly(LogArea,2)+poly(RamanSuit,1)+
                        poly(LogAge,2)+poly(LogGDP,2)+poly(GINI,2)+
                        (1|ISO3_update)+(1|BIOME_NAME),
                   data=data_test)

saveRDS(model_HDslope,
        file = paste0(outDir,"4_model_HDslope.rds")) 

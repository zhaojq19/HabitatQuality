library(xlsx)
library(dplyr)

##### HQ slope #####
outDir <- "E:/PAHQ/ProcessedData/"
alldata <- readRDS("E:/PAHQ/Data/2_HQ_TidyTable_Nonslope_all.rds")

alldata <- subset(alldata, Validyr>5)
tempModel <- apply(alldata[,as.character(seq(1992,2020,1))],
                   1, function(x) lm(x~seq(1992,2020,1)))
alldata$pvalue <- sapply(tempModel, function(x) anova(x)$`Pr(>F)`[1],
                           simplify = TRUE)
alldata$slope <- sapply(tempModel, function(x) x$coefficients[2],
                          simplify = TRUE)
alldata$intercept <- sapply(tempModel, function(x) x$coefficients[1],
                              simplify = TRUE)

saveRDS(object = tempModel,
        file = paste0(outDir,"2_ModelLM_HQ_all.rds"))

saveRDS(object = alldata,
        file = paste0(outDir,"2_HQ_SlopeTable_all.rds"))

##### HD slope #####
alldata <- readRDS("E:/PAHQ/Data/2_HD_TidyTable_Nonslope_all.rds")

alldata <- subset(alldata, Validyr>5)
tempModel <- apply(alldata[,as.character(seq(1992,2020,1))],
                   1, function(x) lm(x~seq(1992,2020,1)))
alldata$pvalue <- sapply(tempModel, function(x) anova(x)$`Pr(>F)`[1],
                         simplify = TRUE)
alldata$slope <- sapply(tempModel, function(x) x$coefficients[2],
                        simplify = TRUE)
alldata$intercept <- sapply(tempModel, function(x) x$coefficients[1],
                            simplify = TRUE)

saveRDS(object = tempModel,
        file = paste0(outDir,"2_ModelLM_HD_all.rds"))

saveRDS(object = alldata,
        file = paste0(outDir,"2_HD_SlopeTable_all.rds"))


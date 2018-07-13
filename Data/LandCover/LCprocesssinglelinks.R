### Load land cover data and aggregate according to categories defined in
### clc_legend_categories.csv 

rm(list=ls())


setwd("/home/hanno/Bioinvasion/EBAspread/Data/GetLandCoverData/Out")


LCdata <- readRDS(file="LandCocer_Roads_50m.rds")
categories <- read.table("../LC/Legend/clc_legend_categories.csv",sep=";",header=T) # load new categories

LCdata$LCtype <- categories$LC_cat_ID[match(LCdata$LC_ID,categories$GRID_CODE)] # assign new categories

newcatLC <- aggregate(LCdata$prop,by=list(LCdata$LCtype,LCdata$LinkID),sum)
colnames(newcatLC) <- c("LCtype","LinkID","LCprop")


# write.table(newcatLC,"LandCover_Roads_50m_newcat.csv")
saveRDS(newcatLC,"LandCover_Roads_50m_newcat.rds")





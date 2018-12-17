rm(list=ls())
library(CASPIAN)
Ship_Travel_Netw<-readOGR(dsn = "~/../Dropbox/AlienSpeciesSpread/Data/ShipTravel_shp",layer="umlbisch_v1_shp", stringsAsFactors = FALSE)
colnames(Ship_Travel_Netw@data)<-c("FromNode","ToNode","Motorized", "Non-motorized", "Total")
Ship_Travel_Netw@data<-Ship_Travel_Netw@data[,1:4]
Ship_Travel_Netw@data[,3]<-as.numeric(Ship_Travel_Netw@data[,3])
Ship_Travel_Netw@data[,4]<-as.numeric(Ship_Travel_Netw@data[,4])
Ship_Travel_Netw$Length<-SpatialLinesLengths(Ship_Travel_Netw,longlat = TRUE)
Ship_Travel_Netw@data$ID<-paste0("W",1:nrow(Ship_Travel_Netw@data))

devtools::use_data(Ship_Travel_Netw,overwrite = TRUE)

plot(border_dataset)
plot(Ship_Travel_Netw,col="blue",add=T)
Ship_coords<-getNodesCoord(Ship_Travel_Netw)
points(Ship_coords[,2:3],col="red")

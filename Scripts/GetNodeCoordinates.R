rm(list=ls())
graphics.off()

library(SearchTrees)
library(rgdal)
library(rgeos)

#setwd("/home/hanno/Bioinvasion/EBAspread/Data/RoadData")

## load shapefile of roads
roads_shp <- readOGR(dsn="../RoadData",layer="20180704_BelastungLkwPkw_reproj",stringsAsFactors=F)

## reproject to lat/long
net_all_proj <- spTransform(roads_shp,"+init=epsg:4326")

# ll <- lineLength(net_all_proj,byid=T)

## extract coordinates from shapefile
res <- lapply(slot(net_all_proj, "lines"), function(x) lapply(slot(x, "Lines"),function(y) slot(y, "coords")))

## extract first/last row of coordinates
from_node <- as.data.frame(do.call("rbind",lapply(lapply(res,"[[",1),function(s) s[1,])),stringsAsFactors=F)
to_node   <- as.data.frame(do.call("rbind",lapply(lapply(res,"[[",1),function(s) s[dim(s)[1],])),stringsAsFactors=F)

from_node$nodeID <- net_all_proj@data$Von_Knoten
to_node$nodeID <- net_all_proj@data$Nach_Knote

allnodes <- merge(unique(from_node),unique(to_node),by="nodeID",all=T)

# check for differences in nodeIDs from from_node and to_node, should not be any match below
which(allnodes$V1.x!=allnodes$V1.y)
which(allnodes$V2.x!=allnodes$V2.y)

## node coordinates
nodeIDs <- allnodes[,1:3]

## fill NAs (some nodes are only to_nodes or only from_nodes)
nodeIDs[is.na(nodeIDs[,2]),2] <- allnodes[is.na(nodeIDs[,2]),4]
nodeIDs[is.na(nodeIDs[,3]),3] <- allnodes[is.na(nodeIDs[,3]),5]


colnames(nodeIDs)[2:3] <- c("Long","Lat")


#### assign regions to nodes ########################

setwd("/home/hanno/Dropbox/AlienSpeciesSpread/Data/TestDataRoad/20180314_Verkehrsbelastungen2015_DTV/gadm36_DEU_shp")

## load administrative boundaries, federal states
states <- readOGR(dsn=file.path(dir_data,"gadm36_DEU_shp"),layer="gadm36_DEU_1",stringsAsFactors=F)

states2 <- SpatialPolygons(states@polygons)
proj4string(states2) <- proj4string(states)

sp_pts <- SpatialPoints(nodeIDs[,2:3])
proj4string(sp_pts) <- proj4string(states)

states_index <- over(sp_pts,states2)

for (i in 1:length(states_index)){
  states_index[states_index==i] <- states@data$NAME_1[i]
}



## load administrative boundaries, Kreis
Kreis <- readOGR(dsn=getwd(),layer="gadm36_DEU_2",stringsAsFactors=F)

Kreis2 <- SpatialPolygons(Kreis@polygons)
proj4string(Kreis2) <- proj4string(Kreis)

sp_pts <- SpatialPoints(nodeIDs[,2:3])
proj4string(sp_pts) <- proj4string(Kreis)

Kreis_index <- over(sp_pts,Kreis2)

for (i in 1:length(Kreis_index)){
  Kreis_index[Kreis_index==i] <- Kreis@data$NAME_2[i]
}


## load administrative boundaries, Gemeinde
Gemeinde <- readOGR(dsn=getwd(),layer="gadm36_DEU_3",stringsAsFactors=F)

Gemeinde2 <- SpatialPolygons(Gemeinde@polygons)
proj4string(Gemeinde2) <- proj4string(Gemeinde)

sp_pts <- SpatialPoints(nodeIDs[,2:3])
proj4string(sp_pts) <- proj4string(Gemeinde)

Gemeinde_index <- over(sp_pts,Gemeinde2)

for (i in 1:length(Gemeinde_index)){
  Gemeinde_index[Gemeinde_index==i] <- Gemeinde@data$NAME_3[i]
}


## load administrative boundaries, Country

setwd("/home/hanno/DATA/Regions/GADM")

Country <- readOGR(dsn=getwd(),layer="gadm28_adm0",stringsAsFactors=F)
ind <- Country@data$UNREGION2=="Europe"
ind[is.na(ind)] <- FALSE
Country <- Country[ind,]

Country2 <- SpatialPolygons(Country@polygons)
proj4string(Country2) <- proj4string(Country)

sp_pts <- SpatialPoints(nodeIDs[,2:3])
proj4string(sp_pts) <- proj4string(Country)

Country_index <- over(sp_pts,Country2)

for (i in 1:length(Country_index)){
  Country_index[Country_index==i] <- Country@data$NAME_ENGLI[i]
}




#### merging everything #######################################################

nodeIDs$Country <- Country_index
nodeIDs$State <- states_index
nodeIDs$Kreis <- Kreis_index
nodeIDs$Gemeinde <- Gemeinde_index

setwd("/home/hanno/Bioinvasion/EBAspread/Model")

# write.table(nodeIDs,"RoadNodeLocations.csv")

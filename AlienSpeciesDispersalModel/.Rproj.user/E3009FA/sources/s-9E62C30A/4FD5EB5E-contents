getNodesCoord<- function(shapeObj){


## extract coordinates from shapefile
res <- lapply(slot(shapeObj, "lines"), function(x) lapply(slot(x, "Lines"),function(y) slot(y, "coords")))

## extract first/last row of coordinates
from_node <- as.data.frame(do.call("rbind",lapply(res,"[[",1)),stringsAsFactors=F)
to_node   <- as.data.frame(do.call("rbind",lapply(lapply(res,"[[",1),function(s) s[dim(s)[1],])),stringsAsFactors=F)

from_node$nodeID <- shapeObj@data$Von_Knoten
to_node$nodeID <- shapeObj@data$Nach_Knote

allnodes <- merge(unique(from_node),unique(to_node),by="nodeID",all=T)

## node coordinates
nodeIDs <- allnodes[,1:3]

## fill NAs (some nodes are only to_nodes or only from_nodes)
nodeIDs[is.na(nodeIDs[,2]),2] <- allnodes[is.na(nodeIDs[,2]),4]
nodeIDs[is.na(nodeIDs[,3]),3] <- allnodes[is.na(nodeIDs[,3]),5]
#
colnames(nodeIDs)[2:3] <- c("Long","Lat")

return(nodeIDs)
}


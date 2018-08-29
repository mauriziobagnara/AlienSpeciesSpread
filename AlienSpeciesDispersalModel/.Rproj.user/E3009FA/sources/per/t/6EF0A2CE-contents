# returns segments ID close to initial coordinates provided by the user.
# the maximum distance for a segment to be considered invaded is set by default at 1000 m in the main model function
# The function calculates a subset of 100 closest segment portions, then calculates their distance from the given coordinates
# and returns the unique IDs of the segments with at least one portion at a distance lower than max_dist.
# It is possible that no segments are close enough to the given coordinates. An Error message or warning should be returned if so!

getNeighbourSegm<- function(shapeObj,init_coords,max_dist){

  ## extract coordinates from shapefile
  res <- lapply(slot(shapeObj, "lines"), function(x) lapply(slot(x, "Lines"),function(y) slot(y, "coords")))
  names(res)<-shapeObj@data$ID
  segsID<-sapply(lapply(res,"[[",1),nrow)

   coords<-as.data.frame(do.call("rbind",lapply(res,"[[",1)),stringsAsFactors=F)
   colnames(coords)<- c("lon","lat")
    coords$ID<-rep(names(segsID),segsID)

    ## create a 'search tree' from coordinates
    tree <- SearchTrees::createTree(coords[,1:2]) # required to calculate the nearest neighbour for each set of coordinates

    ## get ID of nearest segment, the ID is the same as in road_shp (at least, it should be)
    ids<-c()
    for (i in 1:nrow(init_coords)){
    neighbor <- SearchTrees::knnLookup(tree,newdat=init_coords[i,],k=1000) # lookup nearest neighbour, 'k' determines the number of nearest neighbors

    D<-coords[neighbor,]
    D<-as.data.frame(D)
    D$dist<-geosphere::distm(D[,1:2], init_coords[i,],fun=distVincentyEllipsoid)
    ids<-c(ids,as.character(unique(D$ID[D$dist<max_dist])))
    }
    if (length(ids) == 0) stop("No segments found within the specified maximum distance.")
    return(unique(ids))
}


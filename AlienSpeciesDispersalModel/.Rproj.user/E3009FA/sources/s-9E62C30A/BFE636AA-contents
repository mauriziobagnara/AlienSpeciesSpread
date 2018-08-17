getNeighbourSegm<- function(shapeObj,init_coords){

  ## extract coordinates from shapefile
  res <- lapply(slot(shapeObj, "lines"), function(x) lapply(slot(x, "Lines"),function(y) slot(y, "coords")))
  names(res)<-shapeObj@data$ID
  segsID<-sapply(lapply(res,"[[",1),nrow)

    coords<-as.data.frame(do.call("rbind",lapply(res,"[[",1)),stringsAsFactors=F)
   coords$ID<-rep(names(segsID),segsID)

    library(SearchTrees)


    ## create a 'search tree' from coordinates
    tree <- createTree(coords[,1:2]) # required to calculate the nearest neighbour for each set of coordinates

    ## get ID of nearest segment, the ID is the same as in road_shp (at least, it should be)
    ids<-c()
    for (i in 1:nrow(init_coords)){
    neighbor <- knnLookup(tree,newdat=init_coords[i,],k=1) # lookup nearest neighbour, 'k' determines the number of nearest neighbors
    #needs to be changed to something distance related? e.g. see package "geosphere"

    ids<-c(ids,coords[neighbor,3])
    }
    return(ids)
}


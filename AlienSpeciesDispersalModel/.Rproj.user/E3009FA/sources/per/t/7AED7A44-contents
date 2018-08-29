plotResults<-function(list_results,dir_data,shapeObj,save_dir){
 #  border_shp <- readOGR(dsn=file.path(dir_data,"gadm36_DEU_shp"),layer="gadm36_DEU_1",stringsAsFactors = F)

  #create palette
  norm<-seq(0,1,length.out=100001)
  color <- rev(colorRampPalette(c("red","orange","yellow"))(100001))
  pal<-as.data.frame(cbind(norm,color))
  pal$color<-as.character(pal$color)
  pal$color[pal$norm==0]<-"lightgray"

  #plotting
  for (i in 1:length(list_results)){
    cat("\n Creating map ",i,"out of ", length(list_results),"\n")

    shapeObj@data <-list_results[[i]]
    shapeObj@data$norm <- as.factor(round(shapeObj@data$Pinv,5))

    shapeObj@data<-merge(shapeObj@data,pal,by="norm",all.x=TRUE,sort=FALSE)


    #roads_shp_sub <- subset(roads_shp,ID%in%road_netw[Pinv>0,ID]) ## create the shapefile subset

    # node_shp_sub <- subset(nodes_shp,Knoten_Num%in%node_state[state==0,FromNode])
    #
    # dat_nodes <- nodes_shp_sub@data
    # dat_nodes <- cbind(dat_nodes,1:dim(dat_nodes)[1])
    # colnames(dat_nodes)[2] <- "order"
    # dat_nodes$Knoten_Num <- as.numeric(dat_nodes$Knoten_Num)
    # nodes_col <- merge(node_state,dat_nodes,by.x="FromNode",by.y="Knoten_Num",all.y=T)

    # nodes_col <- nodes_col[order(nodes_col$order)]

    #   assign(x = "node_shp_sub",value = node_shp_sub, envir = .GlobalEnv)

    png(filename = file.path(save_dir,(paste0("SpreadModel_map",sprintf("%04d", as.numeric(names(list_results)[i])),".png"))),width=7,height=8,units = "in",res=c(3*72))
    # x11(width=7,height = 8)
     op <- par(mar=rep(0,4))
    plot(border_dataset,axes=F,
         panel.first=rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4]),lwd=0.1)
    plot(shapeObj,add=T,col=shapeObj@data$color)

    # for (j in init_nodes) points(subset(node_shp_sub,Knoten_Num%in%j),pch=1,cex=1,col="blue",lwd=2)
    # mtext(t,side=3,line=-2)
    legend("topleft",c(paste0("Iter. #",names(list_results)[i])),box.col = "white",bg = "white")
    plot(border_dataset,axes=F,add=T,lwd=0.1)
    dev.off()
  }
}



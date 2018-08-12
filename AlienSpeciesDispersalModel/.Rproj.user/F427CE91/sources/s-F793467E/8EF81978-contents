plotResults<-function(list_results,dir_data,netw_data,data_plot,save_dir){

  cat("\n Loading maps data \n")
  if (file.exists(file.path(dir_data,data_plot))) load(file.path(dir_data,data_plot))
  else {
    border_shp <- readOGR(dsn=file.path(dir_data,"gadm36_DEU_shp"),layer="gadm36_DEU_1",stringsAsFactors = F)

    roads_shp <- readOGR(dsn=dir_data,layer=netw_data,stringsAsFactors = F)
    #nodes_shp <- readOGR(dsn=dir_data,layer="20180209_KnotenNemobfstr",stringsAsFactors = F)

    cat("\n Converting coordinates to WGS84")
    cat("\n")
    roads_shp<-spTransform(roads_shp, CRS("+proj=longlat +datum=WGS84"))
    roads_shp@data$ID<-paste(roads_shp@data$Von_Knoten,roads_shp@data$Nach_Knote,sep="_")
    #  nodes_shp<-spTransform(nodes_shp, CRS("+proj=longlat +datum=WGS84"))

    save(border_shp,roads_shp,
         #nodes_shp,
         file=file.path(dir_data,"road_shp.Rdata"))
  }

for (i in 1:length(list_results)){
    cat("\n Creating map ",i,"out of ", length(list_results),"\n")
    colour <- rev(colorRampPalette(c("red","orange","yellow","lightgray"))(101))
    roads_shp@data <- list_results[[i]]
    roads_shp@data$norm <- log(roads_shp@data$Pinv+1)
    roads_shp@data$norm <- round((roads_shp@data$norm/max(roads_shp@data$norm)*100)+1)
    roads_shp@data$col <- colour[roads_shp@data$norm]

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
    op <- par(mar=rep(0,4))
    plot(border_shp,axes=F,
         panel.first=rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4]),lwd=0.1)
    plot(roads_shp,add=T,col=roads_shp@data$col)

    # for (j in init_nodes) points(subset(node_shp_sub,Knoten_Num%in%j),pch=1,cex=1,col="blue",lwd=2)
    # mtext(t,side=3,line=-2)
    legend("topleft",c(paste0("Iter. #",names(list_results)[i])),box.col = "white",bg = "white")
    plot(border_shp,axes=F,add=T,lwd=0.1)
    dev.off()
  }
}

plotResults<-function(list_results,shapeObj,save_plot,save_dir,data_coords=NULL){
  #  border_shp <- readOGR(dsn=file.path(dir_data,"gadm36_DEU_shp"),layer="gadm36_DEU_1",stringsAsFactors = F)

  num_col<-5
  #create palette
  norm<-seq(0,1,length.out=c(10^num_col+1))
  colfunc <- colorRampPalette(c("green3","gold","darkorange","red3","darkred"))
  color<-colfunc(10^num_col+1)
  pal<-data.table(norm=norm,color=color)
  pal$color<-as.character(pal$color)

  #plotting

  for (i in 1:length(list_results)){
    cat("\n Creating map",i,"out of", length(list_results),"\n")
    time_plot<-proc.time()

    shapeObj@data <- list_results[[i]]
    shapeObj@data$norm <- round(shapeObj@data$Pinv,num_col)

    shapeObj@data<-merge(shapeObj@data,pal,by="norm",all.x=TRUE,sort=FALSE)

    #isolating segments where species has been introduced, not introduced, or has invaded
    Inv<-shapeObj[shapeObj@data$norm>0,]
    Not_inv<-shapeObj[shapeObj@data$norm==0,]

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

    if (save_plot==T) {png(filename = file.path(save_dir,(paste0("SpreadModel_map",sprintf("%04d", as.numeric(names(list_results)[i])),".png"))),width=10,height = 8,units = "in",res=c(3*72))
    } else { x11(width=11,height = 8)
    }

    layout(matrix(1:3,nrow=1),widths=c(0.6,0.15,0.25))

    #actual map plotting
    op <- par(mar=c(0.1,1,0.1,0.5))
    plot(border_dataset,axes=F, border="black",
         panel.first=rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4]))
    plot(Not_inv,add=T,col="darkgray")
    plot(Inv,add=T,col=Inv@data$color)
    # for (j in init_nodes) points(subset(node_shp_sub,Knoten_Num%in%j),pch=1,cex=1,col="darkgrey",lwd=2)
    # mtext(t,side=3,line=-2)
    legend("topleft",c(paste0("Iter. #",names(list_results)[i])),box.col = "white",bg = "white")
    plot(border_dataset,axes=F,add=T, border="black")
   if (!is.null(data_coords)) points(data_coords,cex=2,lwd=3,col="blue")

    # legend part
     op <- par(mar=c(0.1,4.5,0.1,0.5))
    num_legend<-40
    color_legend <- rev(c("black","darkgray", colfunc(num_legend)))
    xl <- 1
    yb <- 1
    xr <- 1.5
    yt <- 2
    plot(NA,type="n",ann=FALSE,xlim=c(1,1.7),ylim=c(1,2),xaxt="n",yaxt="n",bty="n")
    rect(
      xl,
      head(seq(yb,yt,(yt-yb)/(num_legend+2)),-1),
      xr,
      tail(seq(yb,yt,(yt-yb)/(num_legend+2)),-1),
      col= color_legend,
      border = color_legend
    )
    mtext(
      c("Administrative border",
        "Species has not invaded",
        "Species has invaded with probability = Pinv"),
      side=4,
      at= c(rev(seq(yb,yt,(yt-yb)/(num_legend+2)))-c(1/(3*num_legend)))[c(1:3)],
      las=2,cex=0.7)
    mtext(
      paste("Pinv =",seq(1,0,by=-0.1),sep=" "),
      side=2,at= c(seq(yb,yt,(yt-yb)/(num_legend+2))[seq(1,(num_legend+1),by=c(num_legend/10))]),
      las=2,cex=0.7)
    frame()

    if (save_plot==T) dev.off()
    cat("\n Map",i,"completed \n")
    print(proc.time() - time_plot)
  }
}


#plotResults(list_results = results[3],shapeObj = Road_Railway_Network[Road_Railway_Network@data$Typ%in%netw_type,],save_plot = FALSE)

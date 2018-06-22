SpreadModel <- function(dir_data, netw_data,init_nodes, num_iter, incl_attachment=T,incl_airflow=T,
                        pickup_prob, att1=NULL,att2=NULL,att3=NULL, air1=NULL,air2=NULL,
                        makeplot=F,data_plot=NULL, iter_plot=NULL){
  ####################################################################

  ### load shapefiles (takes a while!) ######################################################
  ### load shapefiles (takes a while!) ######################################################


  cat("\n Loading network \n")

  road_netw <- st_read(dsn=dir_data,layer="20180314_Verkehrsbelastungen2015_DTV",stringsAsFactors = F)
  road_netw <- as.data.table(road_netw)

  road_netw <- road_netw[,.(Von_Knoten,Nach_Knote,Laenge,DTVKfzMode)] #This syntax throw an error when incorporated into a Function
  names(road_netw) <- c("FromNode","ToNode","Length","Traffic")


  ## add opposite direction (only mean values in both directions are provided so far)
  road_netw_otherdir <- road_netw
  names(road_netw_otherdir) <- c("FromNode","ToNode","Length","Traffic")
  road_netw_otherdir[,ToNode:=road_netw[,FromNode]]
  road_netw_otherdir[,FromNode:=road_netw[,ToNode]]
  road_netw <- rbind(road_netw,road_netw_otherdir)

  ## transform measures into a single dispersal probability
  # Events are considered non mutually exclusive.

  #road_netw[,disp:=0]

  # if (include_traffic) {road_netw[,p_traff:=f_traff(Traffic,traf1)]
  # road_netw[,disp:=disp+p_traff]
  # }

  if (incl_attachment) road_netw[,p_attach:=f_attach(Length,att1,att2,att3,p=1)]

  if (incl_airflow) road_netw[,p_airflow:=f_airflow(Length,air1,air2,p=1)]

  col_prob<-road_netw[,.SD,.SDcols=grep("p_",colnames(road_netw))]

  road_netw[,disp:=apply(col_prob,1,pUnion)]#prod by row
  road_netw[,disp:= 1-(1-disp)^(Traffic*pickup_prob)]

  ## set data.table key for road network (much faster)
  setkey(road_netw,FromNode)

  ##### start simulation ############################################

  ### node file #####################################
  node_state <- as.data.table(unique(c(road_netw[,unique(FromNode)],road_netw[,unique(ToNode)])))
  node_state[,state:=0]
  node_state[,newarrivals:=0]
  names(node_state) <- c("FromNode","state","newarrivals")
  node_state[,newarrivals:=as.numeric(newarrivals)]
  setkey(node_state,FromNode)

  ### initialising simulation #######################

  node_state[FromNode%in%init_nodes,state:=1]

  ### select next nodes #############################

  # ## first step ####
  # nextnodes <- road_netw[FromNode%in%node_state[state>0,FromNode]] # identify next nodes
  # nextnodes <- nextnodes[node_state, nomatch=0] # get states of all nodes
  # newstate <- nextnodes$state * a0 * nextnodes$Length * nextnodes$Traffic # prob to reach nodes
  # node_state[FromNode%in%nextnodes$ToNode,state:=newstate] # assigne new values

  if (makeplot){
    cat("\n Loading data for plot \n")
      if (file.exists(file.path(dir_data,data_plot))) load(file.path(dir_data,data_plot))
      else {
        border_shp <- readOGR(dsn=file.path(dir_data,"gadm36_DEU_shp"),layer="gadm36_DEU_shp",stringsAsFactors = F)

        roads_shp <- readOGR(dsn=dir_data,layer="20180314_Verkehrsbelastungen2015_DTV",stringsAsFactors = F)
        nodes_shp <- readOGR(dsn=dir_data,layer="20180209_KnotenNemobfstr",stringsAsFactors = F)

        cat("\n Converting coordinates to WGS84")
        cat("\n")
        roads_shp<-spTransform(roads_shp, CRS("+proj=longlat +datum=WGS84"))
        nodes_shp<-spTransform(nodes_shp, CRS("+proj=longlat +datum=WGS84"))

        save(border_shp,roads_shp,nodes_shp,file=file.path(dir_data,"road_shp.Rdata"))
      }

      colour <- rev(colorRampPalette(c("red","orange","yellow"))(101))

    #  graphics.off()
    x11(width=7,height=8)
    op <- par(mar=rep(0,4))
    cat("\n Plotting network \n")
    plot(border_shp,axes=F,
         panel.first=rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4]))

    plot(roads_shp,add=T,col="gray")
  }


  ## subsequent steps ######
  cat("\n Ongoing Simulation \n")
  x <- 0

  #setup progress bar
  pb <- txtProgressBar(title = "Simulation state", label = "0% done",min=0, max=num_iter, initial=0,style = 3)

  tmp <- proc.time()
  for (t in 1:num_iter){

    node_state_sub <- node_state[state>0,] # take a subset of occupied nodes, required to speed up 'merge' below
    nextnodes <- road_netw[FromNode%in%node_state_sub$FromNode] # identify next nodes

    nextnodes <- merge(nextnodes,node_state_sub,by="FromNode") # merge old states and next nodes

    newstate <- nextnodes[,1-prod(1-(state * disp)),by=ToNode] # combine all probs arriving at the same node

    node_state[.(newstate$ToNode),newarrivals:=newstate$V1] # add new state to nodes file
    node_state[,state:=1-((1-state)*(1-newarrivals))]   # combine old and new state


    ## 15.06.2018 Maurizio: modified previous plot comment to plot entire German network and borders. Slower,
    ## but does not require a subset of the road shapefile.

    if (makeplot){
      x <- x + 1

      if (t%in%iter_plot){ # plot only few steps
        # roads_shp_sub <- subset(roads_shp,Von_Knoten%in%node_state[state>0,FromNode]) ## create the shapefile subset, not ideal...

        node_shp_sub <- subset(nodes_shp,Knoten_Num%in%node_state[state>0,FromNode])

        dat_nodes <- node_shp_sub@data
        dat_nodes <- cbind(dat_nodes,1:dim(dat_nodes)[1])
        colnames(dat_nodes)[2] <- "order"
        dat_nodes$Knoten_Num <- as.numeric(dat_nodes$Knoten_Num)
        nodes_col <- merge(node_state,dat_nodes,by.x="FromNode",by.y="Knoten_Num",all.y=T)
        nodes_col$norm <- log(nodes_col$state+1)
        nodes_col$norm <- round((nodes_col$norm/max(nodes_col$norm)*100)+1)
        nodes_col$col <- colour[nodes_col$norm]
        nodes_col <- nodes_col[order(nodes_col$order)]

        points(node_shp_sub,col=nodes_col$col,pch=16,cex=0.6)
        for (j in init_nodes) points(subset(node_shp_sub,Knoten_Num%in%init_nodes),pch=1,cex=1,col="blue",lwd=2)
        # mtext(t,side=3,line=-2)
        legend("topleft",c(paste0("Iter. #",t)),box.col = "white",bg = "white")
        plot(border_shp,axes=F,add=T)

        # dev.copy(png,file=paste0("C:/Users/mbagnara/Desktop/SpreadModelIter",sprintf("%04d", t),".png"))
        # dev.off()

      }
    }
    #update progress bar
    info <- sprintf("%d%% done", round((t/num_iter)*100))
    setTxtProgressBar(pb, t/(100)*100, label=info)
  }
  close(pb)

  cat("\n Simulation complete \n")
  print(proc.time() - tmp)

return(road_netw)

}

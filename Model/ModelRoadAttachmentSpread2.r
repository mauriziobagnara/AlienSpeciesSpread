# Model to simulate the spread of propagules attached to motor vehicles
#
# requires the road network: 20180314_Verkehrsbelastungen2015_DTV (shapefile)
#############################################################################

rm(list=ls())
graphics.off()

library(rgdal) # only used for plotting
library(sf)
library(data.table)

setwd("/home/hanno/Bioinvasion/EBAspread/Data/RoadData")

makeplot <- T # plot spread? 

#### Model paramters and functions #################################

## pick-up probability
a0 <- 0.1 # 

## dispersal kernel
b <- -0.05
c <- 0.6556
g <- 0.3311
f_disp <- function(D) exp(b*exp(c*(D^g)))

## traffic kernel
a <- 1e-06
f_traff <- function(T) 1-exp(-a*T)

####################################################################

### load shapefiles (takes a while!) ######################################################
if (makeplot){
  roads_shp <- readOGR(dsn=getwd(),layer="20180314_Verkehrsbelastungen2015_DTV",stringsAsFactors = F)
  nodes_shp <- readOGR(dsn=getwd(),layer="20180209_KnotenNemobfstr",stringsAsFactors = F)

  colour <- rev(colorRampPalette(c("red","orange","yellow"))(101))
}


## load network ###################################

road_netw <- st_read(dsn=getwd(),layer="20180314_Verkehrsbelastungen2015_DTV",stringsAsFactors = F)
road_netw <- as.data.table(road_netw)

road_netw <- road_netw[,.(Von_Knoten,Nach_Knote,Laenge,DTVKfzMode)]
names(road_netw) <- c("FromNode","ToNode","Length","Traffic")


## add opposite direction (only mean values in both directions are provided so far)
road_netw_otherdir <- road_netw
names(road_netw_otherdir) <- c("FromNode","ToNode","Length","Traffic")
road_netw_otherdir[,ToNode:=road_netw[,FromNode]]
road_netw_otherdir[,FromNode:=road_netw[,ToNode]]
road_netw <- rbind(road_netw,road_netw_otherdir)

## transform measures into a single dispersal probability
# road_netw[,Length:=f_disp(Length)]
# road_netw[,Traffic:=f_traff(Traffic)]
road_netw[,disp:=a0 * f_traff(Traffic)*f_disp(Length)]

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
init_node <- 46239
node_state[FromNode%in%init_node,state:=1]

### select next nodes #############################

# ## first step ####
# nextnodes <- road_netw[FromNode%in%node_state[state>0,FromNode]] # identify next nodes
# nextnodes <- nextnodes[node_state, nomatch=0] # get states of all nodes
# newstate <- nextnodes$state * a0 * nextnodes$Length * nextnodes$Traffic # prob to reach nodes
# node_state[FromNode%in%nextnodes$ToNode,state:=newstate] # assigne new values

if (makeplot){
  graphics.off()
  x11(width=10,height=8)
  op <- par(mar=rep(0,4))
  plot(roads_shp_sub,axes=T,
       panel.first=rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = gray(0.95)))
}


## subsequent steps ######

max_times <- 1000 # simulation steps
plot_times <- c(1,2,3,4,5,6,7,8,9,round(seq(10,max_times,length.out=10))) # used for plotting
x <- 0

tmp <- proc.time()
for (t in 1:max_times){

  node_state_sub <- node_state[state>0,] # take a subset of occupied nodes, required to speed up 'merge' below
  nextnodes <- road_netw[FromNode%in%node_state_sub$FromNode] # identify next nodes

  nextnodes <- merge(nextnodes,node_state_sub,by="FromNode") # merge old states and next nodes

  newstate <- nextnodes[,1-prod(1-(state * disp)),by=ToNode] # combine all probs arriving at the same node
  
  node_state[.(newstate$ToNode),newarrivals:=newstate$V1] # add new state to nodes file
  node_state[,state:=1-((1-state)*(1-newarrivals))]   # combine old and new state
  

  ### plotting (not ideal as it requires a subset of the road shapefile, which can only be calculated at the end of the simulation; to make the plot the simulation has to be repeated with the now availabe shapefile subet)
  if (makeplot){
    x <- x + 1
    if (t%in%plot_times){ # plot only few steps
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
      points(subset(node_shp_sub,Knoten_Num==init_node),pch=1,cex=1,col="blue",lwd=2)
      mtext(t,side=3,line=-2)
    }
  }
}
if (makeplot) par(op)

print(proc.time() - tmp)



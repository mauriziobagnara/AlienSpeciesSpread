# Model to simulate the spread of propagules attached to motor vehicles
#
# requires the road network: 20180314_Verkehrsbelastungen2015_DTV (shapefile)
#############################################################################

rm(list=ls())
#graphics.off()

library(rgdal) # only used for plotting
library(sf)
library(data.table)

setwd("C:/Users/mbagnara/Desktop/BiK-F postDoc/Data/TestDataRoad/20180314_Verkehrsbelastungen2015_DTV/")

makeplot <- TRUE # plot spread? 

#### Model parameters and functions #####
cat("\n Defining Kernels")

## dispersal kernel
a0 <- 0.001  ## pick-up probability:  can be improved using function based on seed characteristics (e.g. seed mass)
a <- 0.6556 
b <- -0.05
g <- 0.3311
f_disp <- function(D,a,b,g,p) exp(b*exp(a*(D^g)))*p # model 2 in Taylor et al 2012, requires distance in km

## traffic kernel
d <- 1e-06
f_traff <- function(T,a) 1-exp(-a*T)

## add airflow kernel
e<-0.211
f<-0.791
f_airflow<-function (D,a,b)  1/(sqrt(2*pi)*a*D)*exp(-((abs(log(D) - b))^2 / (2*a^2)))# lognormal, Von der Lippe et al. 2013, required D in meters

## add commodities

## add cabin kernel

## Selecting included drivers
distance_incl <- TRUE
traffic_incl <- TRUE
airflow_incl <- TRUE

####################################################################
cat("\n Loading shapefiles \n")


### load shapefiles (takes a while!) ######################################################
if (makeplot){
  if (file.exists("road_shp.Rdata")) load("road_shp.Rdata")
  else {
    setwd("C:/Users/mbagnara/Desktop/BiK-F postDoc/Data/gadm36_DEU_shp/")
    border_shp <- readOGR(dsn=getwd(),layer="gadm36_DEU_1",stringsAsFactors = F)
    setwd("C:/Users/mbagnara/Desktop/BiK-F postDoc/Data/germany-places-shape/")
    places_shp <- readOGR(dsn=getwd(),layer="places",stringsAsFactors = F)
    
    setwd("C:/Users/mbagnara/Desktop/BiK-F postDoc/Data/TestDataRoad/20180314_Verkehrsbelastungen2015_DTV/")
    roads_shp <- readOGR(dsn=getwd(),layer="20180314_Verkehrsbelastungen2015_DTV",stringsAsFactors = F)
    nodes_shp <- readOGR(dsn=getwd(),layer="20180209_KnotenNemobfstr",stringsAsFactors = F)
    
    cat("\n Converting coordinates to WGS84")
    cat("\n")
    roads_shp<-spTransform(roads_shp, CRS("+proj=longlat +datum=WGS84"))
    nodes_shp<-spTransform(nodes_shp, CRS("+proj=longlat +datum=WGS84"))
    
    save(border_shp,roads_shp,nodes_shp,places_shp,file="road_shp.Rdata")
  }
  
  colour <- rev(colorRampPalette(c("red","orange","yellow"))(101))
  
}

## load network #################################
road_netw <- st_read(dsn=getwd(),layer="20180314_Verkehrsbelastungen2015_DTV",stringsAsFactors = F)
road_netw<-st_transform(road_netw, crs= "+proj=longlat +datum=WGS84")
road_netw <- as.data.table(road_netw)

road_netw <- road_netw[,.(Von_Knoten,Nach_Knote,Laenge,DTVKfzMode)]
names(road_netw) <- c("FromNode","ToNode","Length","Traffic")



## add opposite direction (only mean values in both directions are provided so far)
road_netw_otherdir <- road_netw
names(road_netw_otherdir) <- c("FromNode","ToNode","Length","Traffic")
road_netw_otherdir[,ToNode:=road_netw[,FromNode]]
road_netw_otherdir[,FromNode:=road_netw[,ToNode]]
road_netw <- rbind(road_netw,road_netw_otherdir)

## set key for data.table
setkey(road_netw,FromNode)

## transform measures into probabilities
if (distance_incl) road_netw[,Attach_prob:=f_disp(Length,a,b,g,a0)]
if (traffic_incl) road_netw[,Traffic_prob:=f_traff(Traffic,d)]
if (airflow_incl) road_netw[,Airflow_prob:=f_airflow(Length*1000,e,f)]


##### start simulation ############################################
cat("\n Initialising simulation")
### node file #####################################
node_state <- as.data.table(unique(c(road_netw[,unique(FromNode)],road_netw[,unique(ToNode)])))
node_state <- cbind(node_state,0)
names(node_state) <- c("FromNode","state")
setkey(node_state,FromNode)

### initialising simulation #######################
init_node <- 46239 #somewhere in Berlin?
#init_node <- 205131 # or 207036 Somewhere close to Freiburg

node_state[FromNode==init_node,state:=1]

### select next nodes #############################

## first step ####
nextnodes <- road_netw[FromNode%in%node_state[state>0,FromNode]] # identify next nodes
nextnodes <- nextnodes[node_state, nomatch=0] # get states of all nodes

newstate <- nextnodes$state       
if (distance_incl) newstate <- newstate * nextnodes$Attach_prob   # prob to reach nodes due to attachment including pick-up probability 
if (traffic_incl) newstate <- newstate  * nextnodes$Traffic_prob  # prob to reach nodes due to traffic amount
if (airflow_incl) newstate <- newstate * nextnodes$Airflow_prob   # prob to reach nodes due to vehicle airflow

node_state[FromNode%in%nextnodes$ToNode,state:=newstate] # assigne new values
nodelist<-nextnodes$ToNode
roads_shp_sub <- subset(roads_shp,Von_Knoten%in%nodelist)

cat("\n Drawing map")
if (makeplot){
  #graphics.off()
  x11(width=7,height=8)
  op <- par(mar=rep(0,4))
  plot(border_shp,axes=F,
       panel.first=rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4]))
  
  plot(roads_shp,add=T,col="gray")
}

cat("\n Ongoing Simulation \n")
## subsequent steps ######
max_times <- 365*1 # simulation steps
max_node<-10^10 # max number of nodes infected: if higher, the simulation stops

plot_times <- round(c(seq(1,max_times,length.out=25),max_times)) # used for plotting


x <- 0
# tmp <- tempfile()
# Rprof(tmp, interval = 0.1)
# tmp <- proc.time()

#setup progress bar

pb <- txtProgressBar(title = "Simulation state", label = "0% done",min=0, max=max_times, initial=0,style = 3)

for (t in 1:max_times){
  ninfected <- node_state[state>0,FromNode]
  if (length(ninfected)>max_node) break  # stop if too many nodes are infected
  
  nextnodes <- road_netw[FromNode%in%node_state[state>0,FromNode]] # identify next nodes
  nextnodes <- nextnodes[node_state, nomatch=0] # merge with node_state to get states of all relevant nodes
  nextnodes[,newstate:=nextnodes$state * nextnodes$Attach_prob * nextnodes$Traffic_prob * nextnodes$Airflow_prob] # prob to reach nodes
  
  newstate <- nextnodes[,1-prod(1-newstate),by=ToNode] # combine all arrivals to the same node
  names(newstate) <- c("FromNode","finalarrivals")
  setkey(newstate,FromNode)
  
  old_new_states <- node_state[newstate]
  old_new_states[,finalstate:=1-((1-state)*(1-finalarrivals))] # merge new node state with with old state
  
  node_state[FromNode%in%old_new_states$FromNode,state:=old_new_states$finalstate] # assigne new values
  
  if (makeplot){
    x <- x + 1
    if (t%in%plot_times){ # plot only few steps
      nodelist<-nextnodes$ToNode
      roads_shp_sub <- subset(roads_shp,Von_Knoten%in%nodelist)
      node_shp_sub <- subset(nodes_shp,Knoten_Num%in%node_state[state>0,FromNode])
      
      dat_nodes <- node_shp_sub@data
      dat_nodes <- cbind(dat_nodes,1:dim(dat_nodes)[1])
      dat_nodes$Knoten_Num <- as.numeric(dat_nodes$Knoten_Num)
      nodes_col <- merge(node_state,dat_nodes,by.x="FromNode",by.y="Knoten_Num")
      nodes_col$norm <- log(nodes_col$state+1)
      nodes_col$norm <- round((nodes_col$norm/max(nodes_col$norm)*100)+1)
      nodes_col$col <- colour[nodes_col$norm]
      
      points(node_shp_sub,col=nodes_col$col,pch=16,cex=.1)
      points(subset(node_shp_sub,Knoten_Num==init_node),pch=4,cex=1,col="blue",lwd=2)
      
      legend("topleft",c(paste0("Iter. #",t)),box.col = "white",bg = "white")
    }
  }
  
  #update progress bar
  info <- sprintf("%d%% done", round((t/max_times)*100))
  setTxtProgressBar(pb, t/(100)*100, label=info)
  
}

plot(border_shp,axes=F,add=T)
# places_shp_sub<-subset(places_shp,population>300000)
# Encoding(places_shp_sub$name) <- "UTF-8"
# text(places_shp_sub, labels = places_shp_sub$name,cex=.8)
# plot(places_shp_sub,axes=F,add=T,pch=16,cex=.6)

###### Closing the Progress Bar
close(pb)

cat("\n Plotting")
if (makeplot) par(op)

# Rprof(NULL)
# summaryRprof(tmp)
# proc.time() - tmp
if (length(ninfected)>max_node) cat("\n More then ",max_node, "infected, simulation halted") 
cat("\n Simulation complete")
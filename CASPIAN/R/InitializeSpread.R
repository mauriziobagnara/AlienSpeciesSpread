InitializeSpread<-function(internal_dataset=TRUE, save_init=TRUE, file_init,netw_type=c("all"),
                     dir_data=NULL, netw_data=NULL,Rdata_file=NULL,init_coords,max_dist,save_dir,
                     species_preferences,traffic_type=c("all")){

### load shapefiles (takes a while!) ######################################################
tmp2 <- proc.time()

cat("\n Loading network \n")
if (internal_dataset) { cat("\n Using internal database \n")
  roads_shp<-Road_Railway_Network
} else if (file.exists(file.path(dir_data,Rdata_file))) {
  load(file.path(dir_data,Rdata_file))
} else {

  roads_shp <- readOGR(dsn=dir_data,layer=netw_data,stringsAsFactors = F)
  #nodes_shp <- readOGR(dsn=dir_data,layer="20180209_KnotenNemobfstr",stringsAsFactors = F)

  cat("\n Converting coordinates to WGS84")
  cat("\n")
  roads_shp<-spTransform(roads_shp, CRS("+proj=longlat +datum=WGS84"))
  roads_shp@data$ID<-paste(roads_shp@data$Von_Knoten,roads_shp@data$Nach_Knote,sep="_")
  #  nodes_shp<-spTransform(nodes_shp, CRS("+proj=longlat +datum=WGS84"))

  save(roads_shp,
       #nodes_shp,
       file=file.path(dir_data,"road_shp.Rdata"))
}

#roads_shp@data$ID<-paste(roads_shp@data$Von_Knoten,roads_shp@data$Nach_Knote,sep="_")
#roads_shp@data[, c(4,6,7)]<-sapply(roads_shp@data[, c(4,6,7)], as.numeric)
colnames(roads_shp@data) <- c("FromNode","ToNode","Type","Length","cargo","passengers", "ID")

if (all(netw_type!=c("all"))) roads_shp<-roads_shp[roads_shp@data$Type%in%netw_type,]

road_netw <- as.data.table(roads_shp@data)
road_netw[,Order:=c(1:nrow(roads_shp@data))]

suppressWarnings(
if (all(traffic_type!=c("all"))) {
  colTraffic<-which(colnames(road_netw)%in%traffic_type)
  road_netw[,Traffic:=rowSums(road_netw[, ..colTraffic])]} else {
                                                  road_netw[,Traffic:=rowSums(cbind(cargo,passengers))]}
)
road_netw[,Traffic:=round((Traffic)*365/12,0)]
set(road_netw, j=which(colnames(road_netw) %in% c("cargo","passengers")), value=NULL )

#road_netw <- road_netw[,.(Von_Knoten,Nach_Knote,Laenge,Typ, Traffic,ID)]

## add opposite direction (only mean values in both directions are provided so far)
# road_netw_otherdir <- road_netw
# names(road_netw_otherdir) <- c("FromNode","ToNode","Length","Type","Traffic")
# road_netw_otherdir[,ToNode:=road_netw[,FromNode]]
# road_netw_otherdir[,FromNode:=road_netw[,ToNode]]
# road_netw <- rbind(road_netw,road_netw_otherdir)

## transform measures into a single dispersal probability
# Events are considered non mutually exclusive.

#road_netw[,disp:=0]

# if (include_traffic) {road_netw[,p_traff:=f_traff(Traffic,traf1)]
# road_netw[,disp:=disp+p_traff]
# }

### node file #####################################
cat("\n Initialising node states \n")
node_state <- as.data.table(unique(c(road_netw[,unique(FromNode)],road_netw[,unique(ToNode)])))
node_state[,state:=0]
node_state[,newarrivals:=0]
names(node_state) <- c("FromNode","state","newarrivals")
node_state[,newarrivals:=as.numeric(newarrivals)]
setkey(node_state,FromNode)


###############################################################
cat("\n Identifying initial invasion segments \n")

init_segm <- getNeighbourSegmCoord(shapeObj=roads_shp,init_coords=init_coords,max_dist=max_dist)

init_nodes <- road_netw[ID%in%init_segm,c(FromNode,ToNode)] # new
node_state[FromNode%in%init_nodes,state:=1]

# road_netw[ID%in%init_segm,Pinv:=1] # new, necessary???

############################################################### # new
cat("\n Calculating suitability of habitats \n")
fpath<-system.file("extdata", package="CASPIAN")
LCdata <- readRDS(file.path(fpath,"LandCover_RailsRoadsInters_50m.rds"))
categories <- read.xlsx(file.path(fpath,"clc_legend_categories.xlsx"),sheet=2) # load new categories
categories <- categories[,c("GRID_CODE","LC_cat_ID")]
categories<-as.data.table(categories)

setkey(categories,LC_cat_ID)
setkey(species_preferences,LC_cat_ID)
categories <- species_preferences[categories]

### assign new land cover categories and species preferences
LCdata$LCtype <- categories$LC_cat_ID[match(LCdata$LC_ID,categories$GRID_CODE)] # assign new categories
LCdata$SpecPref <- categories$Species_preferences[match(LCdata$LC_ID,categories$GRID_CODE)] # assign new categories
LCdata$LCprop <- LCdata$prop * LCdata$SpecPref

## calculate suitability of habitats for each segment
LCdata <- as.data.table(LCdata)
road_segm_suit <- LCdata[,sum(LCprop),by=list(LinkID)]
road_segm_suit[V1>1,V1:=1]

## merge land cover suitability and road_netw
colnames(road_segm_suit) <- c("ID","LCsuit")

setkey(road_segm_suit,ID)
setkey(road_netw,ID)
road_netw <- road_segm_suit[road_netw]


### select next nodes #############################

# ## first step ####
# nextnodes <- road_netw[FromNode%in%node_state[state>0,FromNode]] # identify next nodes
# nextnodes <- nextnodes[node_state, nomatch=0] # get states of all nodes
# newstate <- nextnodes$state * a0 * nextnodes$Length * nextnodes$Traffic # prob to reach nodes
# node_state[FromNode%in%nextnodes$ToNode,state:=newstate] # assigne new values
setkey(road_netw,Order)
roads_shp@data<-road_netw

init_data<-list(roads_shp,node_state,init_segm)
names(init_data)<-c("roads_shp","node_state","init_segm")
if (save_init) save(init_data, file = file.path(save_dir,file_init))
print(proc.time() - tmp2)

return(init_data)
}

#INCOMPLETE

InitializeWaterSpread<-function(Water_netw_data,
                                file_init="water_init_data.Rdata",save_init=TRUE,
                           #netw_type=c("all"),
                     dir_data=NULL, netw_data=NULL,Rdata_file=NULL,init_coords,max_dist,save_dir,
                     #species_preferences,
                     traffic_type=c("all")
                     ){

### load and format shapefiles (takes a while!) ######################################################
tmp2 <- proc.time()

cat("\n Loading network \n")
water_shp<-Water_netw_data

colnames(water_shp@data) <- c("FromNode","ToNode","Motorized", "Non_motorized","Length","ID")

water_netw <- as.data.table(water_shp@data)
water_netw[,Order:=c(1:nrow(water_shp@data))]

suppressWarnings(
if (all(traffic_type!=c("all"))) {
  colTraffic<-which(colnames(water_netw)%in%traffic_type)
  water_netw[,Traffic:=rowSums(water_netw[, ..colTraffic])]} else {
                                                  water_netw[,Traffic:=rowSums(cbind(Motorized,Non_motorized))]}
)
water_netw[,Traffic:=round((Traffic)/12,0)]  # assuming the data are yearly traffic
set(water_netw, j=which(colnames(water_netw) %in% c("Motorized","Non_motorized")), value=NULL )

if (any(water_netw[,Length==0])){ #assign length of 10m to segments with length 0
  options(warn=1)
  water_netw[Length==0,Length:=0.01]
  warning("Links of length 0 detected in Water_netw_data. Their length has been set to 10m. ")
  options(warn=0)
}

#water_netw <- water_netw[,.(Von_Knoten,Nach_Knote,Laenge,Typ, Traffic,ID)]

cat("\n Initializing node states \n")

water_netw[,newarrivals:=0]
water_netw[,newarrivals:=as.numeric(newarrivals)]
water_netw[,stateFromNode:=0]
water_netw[,stateToNode:=0]

###############################################################
cat("\n Identifying initial invasion segments \n")

init_segm <- getNeighbourSegmCoord(shapeObj=water_shp,init_coords=init_coords,max_dist=max_dist)

############################################################### # new
cat("\n Calculating suitability of habitats \n")
# fpath<-system.file("extdata", package="CASPIAN")
# LCdata <- readRDS(file.path(fpath,"LandCover_RailsRoadsInters_50m.rds"))
# categories <- read.xlsx(file.path(fpath,"clc_legend_categories.xlsx"),sheet=2) # load new categories
# categories <- categories[,c("GRID_CODE","LC_cat_ID")]
# categories<-as.data.table(categories)
#
# setkey(categories,LC_cat_ID)
# setkey(species_preferences,LC_cat_ID)
# categories <- species_preferences[categories]
#
# ### assign new land cover categories and species preferences
# LCdata$LCtype <- categories$LC_cat_ID[match(LCdata$LC_ID,categories$GRID_CODE)] # assign new categories
# LCdata$SpecPref <- categories$Species_preferences[match(LCdata$LC_ID,categories$GRID_CODE)] # assign new categories
# LCdata$LCprop <- LCdata$prop * LCdata$SpecPref
#
# ## calculate suitability of habitats for each segment
# LCdata <- as.data.table(LCdata)
# road_segm_suit <- LCdata[,sum(LCprop),by=list(LinkID)]
# road_segm_suit[V1>1,V1:=1]

# ## merge land cover suitability and road_netw
# colnames(road_segm_suit) <- c("ID","LCsuit")

# setkey(road_segm_suit,ID)
# setkey(water_netw,ID)
# road_netw <- road_segm_suit[road_netw]

water_netw[,LCsuit:=1] #assumes maximum suitability in all links. Possible factors to consider for suitability: temperature, salinity, pollution

###########################################################
cat("\n Assembling initialization object \n")
setkey(water_netw,Order)
water_shp@data<-water_netw

init_water_data<-list(water_shp,init_segm)
names(init_water_data)<-c("water_shp","init_segm")

if (save_init) {
  cat("\n Saving initialization object \n")
  save(init_water_data, file = file.path(save_dir,file_init))
}
print(proc.time() - tmp2)

return(init_water_data)
}

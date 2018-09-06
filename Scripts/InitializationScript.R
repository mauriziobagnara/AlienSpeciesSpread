# Model to simulate the spread of propagules attached to motor vehicles
#
# requires the road network: 20180314_Verkehrsbelastungen2015_DTV (shapefile)
#############################################################################

rm(list=ls())
graphics.off()


library(rgdal) # only used for plotting
library(sf)
library(data.table)

mainDir<-"C:/Users/mbagnara/Desktop/BiK-F postDoc/Model"
setwd(mainDir)

#setwd("/home/hanno/Bioinvasion/EBAspread/Data/RoadData")

#### Model parameters #################################

## pick-up probability. For Attachment AND airflow dispersal
par_pickup <- 0.000001#

## attachment kernel parameters
par_att1 <- 0.6556
par_att2 <- -0.05
par_att3 <- 0.3311
#f_attach <- function(D) exp(b*exp(c*(D^g)))

## traffic kernel parameters
# traf1 <- 1e-06
#f_traff <- function(T) 1-exp(-a*T)

## airflow kernel parameters
par_air1<-0.211
par_air2<-0.791

## natural dispersal kernel parameter
par_nat1<- 1.06 # González-Martínez et al . 2006, P. pinaster
par_nat2<- 0.5 #González-Martínez et al . 2006, P. pinaster.   b>1: thin-tailed ; b<1: fat-tailed. Values for b generally found from 0.3 to 0.6 (Nathan et al. 2012)

## establishment scale parameter
par_est<- .5 #arbitrary,<=1. Pioneer species should have high values (more likely to establish if the habitat is suitable), late succession species lower values.

 # build parameter matrix

parameters<-matrix(c(par_pickup,par_att1,par_att2,par_att3,par_air1,par_air2,par_nat1,par_nat2,par_est),nrow=1,byrow=T)
colnames(parameters)<-c("pickup_prob", "att1","att2","att3", "air1","air2","nat1","nat2","scale_est")

#Set landcover IDs suitable for establishment
Suitable_LandCoverID<-c(10:11,12:29) #select all and it should not make a difference

#### Initialization info #################################

dir_data<-"C:/Users/mbagnara/Dropbox/AlienSpeciesSpread/Data/TestDataRoad/20180314_Verkehrsbelastungen2015_DTV"
netw_data<-"20180704_BelastungLkwPkw" #network layer
Rdata_file<- "road_shp.Rdata"


init_coords <-data.frame(Long=c(9.9938531,13.2862487),Lat=c(53.5396466,52.5588327),Iter=c(0,20))  #data.frame(Long=c(9.9938531,13.2862487),Lat=c(53.5396466,52.5588327)) # Hamburg Hafen & Berlin airport


num_iter<- 40 # simulation steps
iter_save <- c(1,20,40) #round(seq(1,num_iter,length.out = 5),0)

road_type <- c("A","B") # types of road considered

#starting from scratch:
modelResults<-SpreadModel(parameters,internal_dataset=TRUE,road_type=road_type,
                          initialize = T, save_init = T, file_init = "init_data.Rdata",
                          init_coords=init_coords, num_iter=num_iter,max_dist = 10^4,
                          incl_attachment=T,incl_airflow=T, LandCoverID=Suitable_LandCoverID,
                          makeplot = T,save_plot=F,iter_save = iter_save
                          )

# #using previously initialization data (will plot only last iteration):
file_init<-"C:/Users/mbagnara/Desktop/BiK-F postDoc/Model/06-Sep-2018 12-06-46/init_data.Rdata" #replace with full path of file created by previous call

modelResults<-SpreadModel(parameters,internal_dataset=TRUE,road_type=road_type,
                          initialize = F, save_init = F, file_init = file_init,
                          num_iter=num_iter,
                          incl_attachment=T,incl_airflow=T, LandCoverID=Suitable_LandCoverID,
                          makeplot = T,save_plot=F,
)

# #starting where we left on the previous simulation (will plot only last iteration and save it):
file_restart = "C:/Users/mbagnara/Desktop/BiK-F postDoc/Model/04-Sep-2018 23-22-39/restart.Rdata" #replace with full path of file created by previous call

modelResults<-SpreadModel(parameters,internal_dataset=TRUE,road_type=road_type,
                          initialize = F,
                          num_iter=num_iter,
                          incl_attachment=T,incl_airflow=T, LandCoverID=Suitable_LandCoverID,
                          makeplot = T,
                          restart=T, file_restart = file_restart
)

 # setwd("C:/Users/mbagnara/Desktop/BiK-F postDoc/Model/31-Aug-2018 22-58-57")
 # system("C:/ImageMagick-7.0.8-Q16/convert.exe -delay 80 *.png ModelSpread.gif")
 # setwd(mainDir)


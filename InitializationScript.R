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
a0 <- 0.000001 #

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

 # build parameter matrix

parameters<-matrix(c(a0,par_att1,par_att2,par_att3,par_air1,par_air2),nrow=1,byrow=T)
colnames(parameters)<-c("pickup_prob", "att1","att2","att3", "air1","air2")

#### Initialization info #################################

dir_data<-"C:/Users/mbagnara/Dropbox/AlienSpeciesSpread/Data/TestDataRoad/20180314_Verkehrsbelastungen2015_DTV"
netw_data<-"20180704_BelastungLkwPkw" #network layer
data_plot<- "road_shp.Rdata"

initNodes <- c(215469)  # Where is this?
init_nodes <- initNodes[1]

num_iter<- 720 # simulation steps
iter_save <- round(seq(1,num_iter,length.out = 5),0) # used for plotting

modelResults<-SpreadModel(parameters,
                          dir_data=dir_data, netw_data=netw_data,init_nodes=init_nodes, num_iter=num_iter,
                          incl_attachment=T,incl_airflow=T,
                          makeplot = T, data_plot = data_plot,iter_save = iter_save)

head(modelResults[[length(modelResults)]],10)

 # setwd("C:/Users/mbagnara/Desktop/BiK-F postDoc/Model/12-Aug-2018 16-25-19")
 # system("C:/ImageMagick-7.0.8-Q16/convert.exe -delay 80 *.png ModelSpread.gif")
 # setwd(mainDir)


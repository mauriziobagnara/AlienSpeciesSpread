# Model to simulate the spread of propagules attached to motor vehicles
#
# requires the road network: 20180314_Verkehrsbelastungen2015_DTV (shapefile)
#############################################################################

rm(list=ls())
graphics.off()


# library(rgdal) # only used for plotting
# library(sf)
# library(data.table)
# library(openxlsx)

# mainDir<-"C:/Users/mbagnara/Desktop/BiK-F postDoc/Model"
mainDir<-"/home/hanno/Bioinvasion/EBAspread/Model"
setwd(mainDir)


# configFile<-file.path(system.file("extdata", package="CASPIAN"),"configFile.R")
configFile<- "/home/hanno/GitHub/AlienSpeciesSpread/CASPIAN/inst/extdata/configFile.R"

#setwd("/home/hanno/Bioinvasion/EBAspread/Data/RoadData")

modelResults<-runCASPIAN(configFile=configFile)

## starting from scratch:
# modelResults<-SpreadModel(parameters,internal_dataset=TRUE,road_type=road_type,
#                           initialize = T, save_init = T, file_init = "init_data.Rdata",
#                           init_coords=init_coords, num_iter=num_iter,max_dist = 10^4,
#                           incl_attachment=T,incl_airflow=T, incl_natural=T,
#                           species_preferences=species_preferences,
#                           makeplot = F,save_plot=F,iter_save = iter_save
#                           )

# #using previously initialization data (will plot only last iteration):
# file_init<-"C:/Users/mbagnara/Desktop/BiK-F postDoc/Model/16-Oct-2018 10-02-39/init_data.Rdata" #replace with full path of file created by previous call
#
# modelResults<-SpreadModel(parameters,internal_dataset=TRUE,road_type=road_type,
#                           initialize = F, save_init = F, file_init = file_init,
#                           num_iter=num_iter,
#                           incl_attachment=T,incl_airflow=T, LandCoverID=Suitable_LandCoverID,
#                           makeplot = F,save_plot=F
# )
#
# # #starting where we left on the previous simulation (will plot only last iteration and save it):
# file_restart = "C:/Users/mbagnara/Desktop/BiK-F postDoc/Model/11-Oct-2018 12-30-45/restart.Rdata" #replace with full path of file created by previous call
#
# modelResults<-SpreadModel(parameters,internal_dataset=TRUE,road_type=road_type,
#                           initialize = F,
#                           num_iter=num_iter,
#                           incl_attachment=T,incl_airflow=T, LandCoverID=Suitable_LandCoverID,
#                           makeplot = T,
#                           restart=T, file_restart = file_restart
# )

 # setwd("C:/Users/mbagnara/Desktop/BiK-F postDoc/Model/31-Aug-2018 22-58-57")
 # system("C:/ImageMagick-7.0.8-Q16/convert.exe -delay 80 *.png ModelSpread.gif")
 # setwd(mainDir)


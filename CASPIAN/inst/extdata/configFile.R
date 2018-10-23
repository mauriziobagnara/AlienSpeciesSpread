#### Model parameters #################################

## attachment kernel parameters
par_att0 <- 0.000001 ## pick-up probability
par_att1 <- 0.6556
par_att2 <- -0.05
par_att3 <- 0.3311
#f_attach <- function(D) exp(b*exp(c*(D^g)))

## traffic kernel parameters
# traf1 <- 1e-06
#f_traff <- function(T) 1-exp(-a*T)

## airflow kernel parameters
par_air0<-0.1
par_air1<-0.211
par_air2<-2

## natural dispersal kernel parameter
par_nat1<- 1.06 # González-Martínez et al . 2006, P. pinaster
par_nat2<- 0.5 #González-Martínez et al . 2006, P. pinaster.   b>1: thin-tailed ; b<1: fat-tailed. Values for b generally found from 0.3 to 0.6 (Nathan et al. 2012)

## establishment scale parameter
par_est<- .5 #arbitrary,<=1. Pioneer species should have high values (more likely to establish if the habitat is suitable), late succession species lower values.


#Set landcover IDs suitability for establishment

Urban_areas	<- 1    #	LC_cat_ID=1
Arable_land	<-	0.5 #LC_cat_ID=2
Pastures	<-	0.1 #LC_cat_ID=3
Forests	<-	0   #LC_cat_ID=4
Wetlands	<-	0   #LC_cat_ID=5


# Suitable_LandCoverID<-c(10:11,12:29) #select all and it should not make a difference

#### Initialization info #################################

# dir_data<-"C:/Users/mbagnara/Dropbox/AlienSpeciesSpread/Data/TestDataRoad/20180314_Verkehrsbelastungen2015_DTV"
# netw_data<-"20180704_BelastungLkwPkw" #network layer
# Rdata_file<- "road_shp.Rdata"


init_coords <-data.frame(Long=c(9.9938531,13.2862487),Lat=c(53.5396466,52.5588327),Iter=c(0,20))  #data.frame(Long=c(9.9938531,13.2862487),Lat=c(53.5396466,52.5588327)) # Hamburg Hafen & Berlin airport


num_iter<- 100 # simulation steps
iter_save <- c(1,20,40) #round(seq(1,num_iter,length.out = 5),0)

road_type <- c("A","B") # types of road considered

internal_dataset<-TRUE  # wheter to use the dataset internally provided with traffic data
initialize<-TRUE  # Whether the model should be initialized.
save_init<-TRUE # if initialize=TRUE, should the initialization file be saved?
file_init<- "init_data.Rdata" # if initialize=TRUE, the name of the file to be created by InitializeSpread().
#   in the newly created folder (default  "init_data.rData" if save_init=TRUE). If initialize=FALSE, the FULL path
#   of the file to be read in (created by InitializeSpread() or ModelSpread() ). MUST BE an .Rdata file.

incl_attachment<-TRUE # if attachment to vehicles should be considered.
incl_airflow<-TRUE # if vehicle airstream should be considered.
incl_natural<-TRUE #if natural dispersal should be considered.

makeplot<-TRUE #should model results be plotted as maps
save_plot<-TRUE # If TRUE, plots are created in the newly created folder as .png files. If FALSE, an x11() device is opened. Only considered if makeplot=TRUE.

save.restart=FALSE #should results be saved in order to resume the simulation at a later stage?
restart=FALSE #Should the simulation be resumed from previously saved results? Results are saved automatically in restart.rData
file_restart=NULL #if restart=TRUE, the FULL path of the file to be read in (previously created by ModelSpread() or runCASPIAN() ).MUST BE an .Rdata file
export_results=FALSE #Should results of the last iteration be exported in the newly created folder as a csv file?

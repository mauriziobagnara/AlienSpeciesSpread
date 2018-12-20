#configFile: full path of the configuration file

runCASPIAN<-function(configFile){

  source(configFile,local=TRUE)

  # build parameter matrix
  parameters<-matrix(c(par_att0_Roads,par_att0_Railways,par_att1,par_att2,par_att3,
                       par_air0_Roads,par_air0_Railways,par_air1,par_air2,
                       par_nat1,par_nat2,par_est_T,par_cont,par_pall,
                       par_mobile_proportion,par_stat_speed,par_mob_speed,
                       par_a,par_c1,par_g,par_c2,par_b,par_c3,par_est_W
                       ),nrow=1,byrow=T)
  colnames(parameters)<-c("S_att0", "R_att0","att1","att2","att3","S_air0","R_air0", "air1","air2","nat1","nat2","estT","cont1","pall1",
                          "mob_prop","stat_speed","mob_speed","a","c1","g","c2","b","c3","estW")

  #build land cover species preference matrix
  species_preferences<- data.table(LC_cat_ID= 1:5,Species_preferences=c(Urban_areas,Arable_land,Pastures,Forests,Wetlands))

  modelResults<-list()

if(runTerrestrialModel==TRUE){
  cat("\n Running Terrestrial Model \n")
  tmp <- proc.time()

  #running model:
  dir.name_T<-file.path(getwd(),paste0("CASPIAN_Terrestrial_",format(Sys.time(), "%d-%b-%Y %H-%M-%S")))
  dir.create(dir.name_T)

  #Model initialization
  if (restart==TRUE){ cat("\n Loading previous results \n")
    load(file_restart)
    init_data<-restart_data
  } else if (restart==FALSE & initialize==TRUE) {
    init_data<-InitializeSpread(Terrestrial_netw_data=Terrestrial_netw_data,
                                Commodities_shape_data=Commodities_shape_data,
                                Pallets_netw_data=Pallets_netw_data,
                                Container_netw_data=Container_netw_data,
                                init_coords=init_coords_T,max_dist=max_dist_T,
                                netw_type=netw_type,save_init=save_init, save_dir=dir.name_T,file_init=file_init,
                                species_preferences=species_preferences,traffic_type=traffic_type_T,
                                incl_containers=incl_containers,incl_pallets=incl_pallets,
                                Cont_treshold=Cont_treshold,Pall_treshold=Cont_treshold)

  } else if (restart==FALSE & initialize==FALSE) {cat("\n Loading initialization data \n")
    load(file_init)
  }


  # Spread Calculations
  TerrestrialModelResults<-SpreadModel(parameters=parameters,init_obj=init_data,
                            Terrestrial_netw_data=Terrestrial_netw_data,
                            Commodities_shape_data=Commodities_shape_data,
                            Pallets_netw_data=Pallets_netw_data,
                            Container_netw_data=Container_netw_data,
                            netw_type=netw_type,traffic_type=traffic_type_T,
                          init_coords=init_coords_T, num_iter=num_iter_T,max_dist = max_dist_T,
                          incl_attachment=incl_attachment,incl_airflow=incl_airflow, incl_natural=incl_natural,
                          incl_containers=incl_containers,incl_pallets=incl_pallets,
                          Cont_treshold=Cont_treshold,Pall_treshold=Cont_treshold,
                          species_preferences=species_preferences,
                          iter_save = iter_save_T
                          )

  cat("\n Model calculation completed \n")
  print(proc.time() - tmp)
  cat("\n Output files being created in ", dir.name_T, "\n")

  if (export_results) {
    cat("\n Exporting final results \n")
    write.csv(x = road_netw_out,file = file.path(dir.name_T, "ModelResults.csv"),quote = F,row.names = F)
    # assign(x = "modelList",value = modelList,envir = .GlobalEnv)
  }

}

if(runAquaticModel==TRUE){
   cat("\n Running Aquatic Model \n")
  tmp <- proc.time()

  #running model:
  dir.name_W<-file.path(getwd(),paste0("CASPIAN_Aquatic_",format(Sys.time(), "%d-%b-%Y %H-%M-%S")))
  dir.create(dir.name_W)

  #Model initialization
  if (restart==TRUE){ cat("\n Loading previous results \n")
    load(file_restart)
    init_water_data<-water_restart_data
  } else if (restart==FALSE & initialize==TRUE) {
    init_water_data<-InitializeWaterSpread(Water_netw_data=Water_netw_data,
                                           init_coords=init_coords_W,max_dist=max_dist_W,
                                           #netw_type=netw_type,
                                           save_init=save_init, save_dir=dir.name_W,file_init=file_init,
                                           #species_preferences=species_preferences,
                                           traffic_type=traffic_type_W
    )

  } else if (restart==FALSE & initialize==FALSE) {cat("\n Loading initialization data \n")
    load(file_init)
  }

  #Spread calculations
  AquaticModelResults<-WaterSpreadModel(parameters=parameters,init_obj=init_water_data,
                                           Water_netw_data=Water_netw_data,
                                         traffic_type=traffic_type_W,
                                         init_coords=init_coords_W, num_iter=num_iter_W,max_dist = max_dist_W,
                                         incl_biofouling=incl_biofouling,incl_natural_water=incl_natural_water,
                                         Port_time=Port_time,Paint_time=Paint_time,
                                         iter_save = iter_save_W
                        )

  cat("\n Model calculation completed \n")
  print(proc.time() - tmp)
  cat("\n Output files being created in ", dir.name_W, "\n")

  if (export_results) {
    cat("\n Exporting final results \n")
    write.csv(x = water_netw,file = file.path(dir.name_W, "ModelResults.csv"),quote = F,row.names = F)
    # assign(x = "modelList",value = modelList,envir = .GlobalEnv)
  }

}

if (makeplot) {
  if (runTerrestrialModel==TRUE){
    cat("\n Creating Terrestrial maps \n")
  plotResults(list_results=TerrestrialModelResults,shapeObj=init_data$roads_shp,save_plot=save_plot,save_dir=dir.name_T)
    }
  if (runAquaticModel==TRUE){
    cat("\n Creating Aquatic maps \n")
  plotResults(list_results=AquaticModelResults,shapeObj=init_water_data$water_shp,save_plot=save_plot,save_dir=dir.name_W)
  }
}

if (runTerrestrialModel==TRUE){
  modelResults[["TerrestrialResults"]]<-TerrestrialModelResults
}
if (runAquaticModel==TRUE){
  modelResults[["AquaticResults"]]<-AquaticModelResults
 }

cat("\n Simulation complete \n")
print(proc.time() - tmp)

return(modelResults)
}

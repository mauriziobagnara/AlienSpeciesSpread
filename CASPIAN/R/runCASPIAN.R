#configFile: full path of the configuration file

runCASPIAN<-function(configFile){

  source(configFile)

  # build parameter matrix

  parameters<-matrix(c(par_att0_Roads,par_att0_Railways,par_att1,par_att2,par_att3,par_air0_Roads,par_air0_Railways,par_air1,par_air2,par_nat1,par_nat2,par_est),nrow=1,byrow=T)
  colnames(parameters)<-c("S_att0", "R_att0","att1","att2","att3","S_air0","R_air0", "air1","air2","nat1","nat2","est0")

  #build land cover species preference matrix
  species_preferences<- data.table(LC_cat_ID= 1:5,Species_preferences=c(Urban_areas,Arable_land,Pastures,Forests,Wetlands))

  #running model:

  modelResults<-SpreadModel(parameters,internal_dataset=internal_dataset,netw_type=netw_type,
                          initialize = initialize, save_init = save_init, file_init = file_init,
                          init_coords=init_coords, num_iter=num_iter,max_dist = 10^4,
                          incl_attachment=incl_attachment,incl_airflow=incl_airflow, incl_natural=incl_natural,
                          species_preferences=species_preferences,
                          makeplot = makeplot,save_plot=save_plot,iter_save = iter_save
)

return(modelResults)
}

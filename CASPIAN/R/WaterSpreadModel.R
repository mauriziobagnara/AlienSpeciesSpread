#INCOMPLETE


#ARGUMENTS:
# parameters: model parameter values. Must be provided in the same order
#   and same form shown in InitializationScript.R. Column names and order matter!
# internal_dataset: wheter to use the dataset internally provided with traffic data. Default TRUE.
# dir_data:if internal_dataset=FALSE, the folder where to look for traffic data. Otherwise ignored.
# netw_data: if internal_dataset=FALSE, the layer of shapefile to be imported. Otherwise ignored.
# Rdata_file: if internal_dataset=FALSE, .Rdata file with the traffic network. Otherwise ignored,
#   alternative to netw_data.
# initialize: Whether the model should be initialized. Default TRUE.
# save_init: if initialize=TRUE, should the initialization file be saved? Default TRUE.
# file_init: if initialize=TRUE, the name of the file to be created by InitializeSpread().
#   in the newly created folder (default  "init_data.rData" if save_init=TRUE). If initialize=FALSE, the FULL path
#   of the file to be read in (created by InitializeSpread() or ModelSpread() ). MUST BE an .Rdata file.
# init_coords: data.table object, with coordinates of invasion starting points and at which point
#   during the simulation they should be considered (set to 0 for consideration from the very beginning).
#   See InitializationScript.R for how to create it.
# num_iter: number of model iterations
# incl_attachment: if attachment to vehicles should be considered. Default TRUE.
# incl_airflow: if vehicle airstream should be considered. Default TRUE.
# incl_natural: if natural dispersal should be considered. Default TRUE.
# LandCoverID: IDs of Suitable Land Cover. See databases LClegend and LClist.
# max_dist: maximum distance (m) from initial coordinates for a segment to be considered infected.
# makeplot: should model results be plotted as maps (could be a long process)? Default FALSE.
# save_plot: logical. If TRUE, plots are created in the newly created folder as .png files.
#   If FALSE, an x11() device is opened. Only considered if makeplot=TRUE. if
# iter_save: which model iterations should be saved? By default=num_iter. If makeplot=TRUE, several maps
#   will be created, one for each saved iteration.
# save.restart: should results be saved in order to resume the simulation at a later stage? Default FALSE
# restart: Should the simulation be resumed from previously saved results? Default FALSE. Results are saved automatically in restart.rData
#   file_restart: if restart=TRUE, the FULL path of the file to be read in (previously created by ModelSpread() ).MUST BE an .Rdata file
# export_results: Should results of the last iteration be exported in the newly created folder as a csv file?
#   Default FALSE.
# netw_type: the types of roads to be considered in the simulation. Default c("all").



WaterSpreadModel <- function(parameters,init_obj,
                             Water_netw_data,
                             dir_data=NULL, netw_data=NULL,Rdata_file=NULL,init_coords, num_iter,
                             incl_ship=T,incl_natural_water=T,
                             #species_preferences,
                             max_dist,Port_time=NA,Paint_time=NA,
                             iter_save=num_iter,plot_funct_rel=FALSE,
                             save.restart=FALSE,restart=FALSE,file_restart=NULL,
                             export_results=F,
                             #netw_type=c("all"),
                             traffic_type=c("all")){

  ####################################################################

  for(i in 1:length(init_obj)) assign(names(init_obj)[i], init_obj[[i]])

  water_netw<-water_shp@data

  ####################################################################
  ## Calculate probabilities for individual pathways

  for (nparset in 1:nrow(parameters)){

    cat("\n Calculating Probability of Introduction for each segment \n")

    ## Calculate natural dispersal #####################
    if (incl_natural_water==TRUE){
      water_netw[,p_natural:=f_natural_water(prop_mob = parameters[nparset,"mob_prop"],stat = parameters[nparset,"stat_speed"],mob=parameters[nparset,"mob_speed"],d=Length )]
    } else{water_netw[,p_natural:=0]}

    ## Calculate dispersal by ships #####################
    if (incl_ship==TRUE) {
      water_netw[,p_ship:=f_ship(a = parameters[nparset,"alpha"],c1 = parameters[nparset,"c1"],
                                             g = parameters[nparset,"gamma"],c2 = parameters[nparset,"c2"],
                                             b = parameters[nparset,"beta"],c3 = parameters[nparset,"c3"],
                                             Dp = Port_time,Qp = Paint_time,VTp = Length)]
    } else{water_netw[,p_ship:=0]}

    water_netw[is.na(p_ship),p_ship:=0]
    water_netw[is.na(p_natural),p_natural:=0]

    ## ERROR check for p_natural
    if (is.numeric(water_netw[,p_natural])==FALSE |
        any(water_netw[,p_natural<0]) | any(water_netw[,p_natural>1])) {
      assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
      stop ("Problem in probability calculations: Pi_container either non-numeric or not in 0:1 range")
    }
    ## ERROR check for p_ship
    if (is.numeric(water_netw[,p_ship])==FALSE |
        any(water_netw[,p_ship<0]) | any(water_netw[,p_ship>1])) {
      assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
      stop ("Problem in probability calculations: p_ship either non-numeric or not in 0:1 range")
    }

    water_netw[, Pi_traffic:=1-Reduce("*", 1-.SD), .SDcols=grep("p_",colnames(water_netw))] # new solution


    ##################################################################
    # caculate probability of establishment for aquatic habitats

    #get PE for segment
    cat("\n Calculating Probability of Establishment for each segment \n")

    water_netw[,Pe:=parameters[nparset,"estW"]*LCsuit] # parameter for scaling down probability of establishment # new
    water_netw[is.na(Pe),Pe:=0]

    ## ERROR check for Pe
    if (is.numeric(water_netw[,Pe])==FALSE |
        any(water_netw[,Pe<0]) | any(water_netw[,Pe>1])) {
      assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
      stop ("Problem in probability calculations: Pe either non-numeric or not in 0:1 range")
    }

    ## set data.table key for road network (much faster)
    # And subset relevant information
    water_netw_details <- water_netw[,c("ID","LCsuit","Length","Traffic","p_natural","p_ship","Order")]
    set( water_netw, j=which(colnames(water_netw) %in% c("LCsuit","Length","Traffic","p_natural","p_ship","Order")), value=NULL ) # new
    setkey(water_netw,FromNode)


    ##################################################################
    ##### start simulation ###########################################
    ##################################################################

    ## subsequent steps ######
    cat("\n Ongoing Simulation \n")

    #setup progress bar
    pb <- txtProgressBar(title = "Simulation state", label = "0% done",min=0, max=num_iter, initial=0,style = 3)

    modelList<- list()

    for (it in 1:num_iter){#num_iter

      ###### network part

      ind <- which(water_netw$stateFromNode>0 & water_netw$stateToNode<1) # select links with non-empty start node and non-filled end node
      water_netw[,newarrivals2:=newarrivals]
      water_netw[ind,newarrivals2:=   1-prod(1-(stateFromNode * Pi_traffic )) ,by=ToNode] # calculate pintro for each link

      ## ERROR check for declining newarrivals
      if (any(water_netw[,newarrivals2<newarrivals]) &
          any((water_netw[newarrivals2<newarrivals,newarrivals-newarrivals2])>10^-15)) {
        assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
        stop ("Problem in spread probability calculations: Decline in newarrivals")
      } else { water_netw[,newarrivals:=newarrivals2] }
      water_netw[,newarrivals2:=NULL]

      water_netw[,stateToNode2:=stateToNode]
      water_netw[ind,stateToNode2:=1-(prod((1-stateToNode) * (1-newarrivals) )),by=ToNode] # update ToNodes with old and new state
      #      water_netw[,stateToNode2:=1-(prod((1-stateToNode2))),by=ToNode] # update ToNodes with old and new state

      ## ERROR check for declining stateToNode
      if (any(water_netw[,stateToNode2<stateToNode]) &
          any((water_netw[stateToNode2<stateToNode,stateToNode-stateToNode2])>10^-15)) {
        assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
        stop ("Problem in spread probability calculations: Decline in stateToNode")
      } else { water_netw[,stateToNode:=stateToNode2] }
      water_netw[,stateToNode2:=NULL]

      newstate <-water_netw[,c("ToNode","stateToNode")] # extract new state of ToNodes to update FromNodes states
      newstate<-as.data.table(aggregate(stateToNode ~ ToNode, newstate, pUnion))
      setnames(newstate,c("FromNode","newstate")) # prepare file for merge (set names and key)
      setkey(newstate,FromNode)
      setkey(water_netw,FromNode)
      water_netw <- merge(water_netw,newstate,by="FromNode",all.x=T)
      water_netw[is.na(newstate),newstate:=0]

      ## ERROR check for declining stateFromNode
      if (any(water_netw[newstate>0,newstate<stateFromNode]) &
          any((water_netw[newstate>0 & newstate<stateFromNode,stateFromNode-newstate])>10^-15) ) {
        assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
        stop ("Problem in spread probability calculations: Decline in stateFromNode")
      }

      # water_netw <- newstate[water_netw] # merge water_netw and newstate to update FromNodes states
      water_netw[newstate>0 ,stateFromNode:=newstate] # assigne new states to FromNodes
      water_netw[,newstate:=NULL] # remove column to avoid columns with the same names


      # store results
      if (it%in%iter_save) {
        water_netw[,p_link:=stateToNode] #calculate probability of link invasion: to be involved in Pinv instead of Pi, or it stays constant in time
        water_netw[,Pinv:=Pe*p_link] # calculate total probability for links
        water_netw[ID%in%init_segm,Pinv:=1] # assigning Pinv=1 for initial links. No effect on Traffic spread dynamics (use nodes).
        setkey(water_netw,ID)
        setkey(water_netw_details,ID)
        water_netw_out <- water_netw_details[water_netw]
        setkey(water_netw_out,Order)

        ## ERROR check for state probabilities
        if (is.numeric(water_netw_out[,stateFromNode])==FALSE | is.numeric(water_netw_out[,stateToNode])==FALSE |
            any(water_netw_out[,stateFromNode<0]) | any(water_netw_out[,stateFromNode>1]) |
            any(water_netw_out[,stateToNode<0]) | any(water_netw_out[,stateToNode>1])) {
          assign(x = "water_netw_out",value = water_netw_out,envir = .GlobalEnv)
          stop ("Problem in probability calculations: Probabilities either non-numeric or not in 0:1 range")
        }

        modelList[[as.character(it)]]<-water_netw_out

      }

      #update progress bar
      info <- sprintf("%d%% done", round((it/num_iter)*100))
      setTxtProgressBar(pb, it/(100)*100, label=info)
    }
  }
  close(pb)

  water_shp@data<-water_netw_out

  if (save.restart){
    cat("\n Assembling and saving restart object \n")

    water_restart_data<-list(water_shp,init_segm)
    names(water_restart_data)<-c("water_shp","init_segm")

    save(water_restart_data, file = file.path(dir.name,"water_restart.Rdata"))
  }

  return(modelList)
}


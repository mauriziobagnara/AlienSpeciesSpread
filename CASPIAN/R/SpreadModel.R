#needs matrix of parameters. should call parameters by name (via colnames(matrix)).
#  Needs parallelization already? If so, folder structure needed to save results of each run separately.
# parallelization not needed yet for parameters, might be useful for networks at a later stage.

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
#   Default 1000.
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
# road_type: the types of roads to be considered in the simulation. Default c("all").



SpreadModel <- function(parameters,internal_dataset=TRUE,initialize=TRUE,file_init="init_data.Rdata",save_init=TRUE,
                        dir_data=NULL, netw_data=NULL,Rdata_file=NULL,init_coords, num_iter,
                        incl_attachment=T,incl_airflow=T,incl_natural=T,
                        species_preferences,max_dist=1000,
                        makeplot=F, save_plot=F, iter_save=num_iter,
                        save.restart=FALSE,restart=FALSE,file_restart=NULL,
                        export_results=F,road_type=c("all")){
  ####################################################################

  tmp <- proc.time()

  dir.name<-file.path(getwd(),format(Sys.time(), "%d-%b-%Y %H-%M-%S"))
  dir.create(dir.name)

  if (restart==TRUE){ cat("\n Loading previous results \n")
    load(file_restart)
    init_data<-restart_data
  } else if (restart==FALSE & initialize==TRUE) {
    init_data<-InitializeSpread(init_coords=init_coords,max_dist=max_dist,
                                road_type=road_type,save_init=save_init, save_dir=dir.name,file_init=file_init,
                                species_preferences=species_preferences)
  } else if (restart==FALSE & initialize==FALSE) {cat("\n Loading initialization data \n")
    load(file_init)
  }
  roads_shp<-init_data$roads_shp
  road_netw<-roads_shp@data
  init_segm<-init_data$init_segm
  node_state<-init_data$node_state

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

  for (nparset in nrow(parameters)){
    #get PI for segment
    cat("\n Calculating Probability of Introduction for each segment \n")

    if (incl_natural){
      road_netw[,p_natural:=f_natural(Length,parameters[nparset,"nat1"],parameters[nparset,"nat2"] )]
    }

    if (incl_attachment) {
      road_netw[,p_attach:=f_attach(Length,parameters[nparset,"att1"],parameters[nparset,"att2"],parameters[nparset,"att3"])]
      road_netw[,p_attach:= 1-(1-p_attach)^(Traffic*parameters[nparset,"att0"])]
    }
    if (incl_airflow) {
      road_netw[,p_airflow:=f_airflow(Length,parameters[nparset,"air1"],parameters[nparset,"air2"])]
      road_netw[,p_airflow:= 1-(1-p_airflow)^(Traffic*parameters[nparset,"air0"])] # new
    }

    road_netw[, Pi:=1-Reduce("*", 1-.SD), .SDcols=grep("p_",colnames(road_netw))] # new solution

    # col_prob<-road_netw[,.SD,.SDcols=grep("p_",colnames(road_netw))] # see above new solution
    #
    # road_netw[,Pi:=apply(col_prob,1,pUnion)] #prod by row

    #get PE for segment

    cat("\n Calculating Probability of Establishment for each segment \n")

# new    # LCprop<-LCproportion(IDs=unique(road_netw$ID),List=LClist,LandCoverID=LandCoverID) #requires LClist, provided as internal data in data/LClist.rda
#    road_netw[,list(road_netw,LCprop)]
# new    if (restart==TRUE){road_netw[,Pe:=NULL]}

# new    road_netw<- merge(road_netw,LCprop,by="ID", all=TRUE,sort=FALSE)
     road_netw[,Pe:=1-exp(-parameters[nparset,"est0"]*LCsuit)] # parameter for scaling down probability of establishment # new


    #road_netw[,Pe:=LCproportion(List=LCList,LandCoverID=LandCoverID)] #for test only! Needs additional merge() to match segment ID

    ## set data.table key for road network (much faster)
    road_netw_details <- road_netw[,c("ID","LCsuit","Length","Traffic","p_natural","p_attach","p_airflow")]
    set( road_netw, j=which(colnames(road_netw) %in% c("LCsuit","Length","Type","Traffic","p_natural","p_attach","p_airflow")), value=NULL ) # new
    setkey(road_netw,FromNode)
    road_netw[,stateFromNode:=0]          # state of FromNode
    road_netw[,stateToNode:=0]    # state of ToNode
    road_netw[,newarrivals:=0]    # pintro for single links
    road_netw[FromNode%in%node_state[state>0,FromNode],stateFromNode:=1] # initialise states of FromNodes
    road_netw[ToNode%in%node_state[state>0,FromNode],stateToNode:=1] # initialise states of ToNodes


    ##### start simulation ############################################

    ### select next nodes #############################

    # ## first step ####
    # nextnodes <- road_netw[FromNode%in%node_state[state>0,FromNode]] # identify next nodes
    # nextnodes <- nextnodes[node_state, nomatch=0] # get states of all nodes
    # newstate <- nextnodes$state * a0 * nextnodes$Length * nextnodes$Traffic # prob to reach nodes
    # node_state[FromNode%in%nextnodes$ToNode,state:=newstate] # assigne new values


    ## subsequent steps ######
    cat("\n Ongoing Simulation \n")

    #setup progress bar
    pb <- txtProgressBar(title = "Simulation state", label = "0% done",min=0, max=num_iter, initial=0,style = 3)

    modelList<- list()

    for (t in 1:num_iter){#num_iter

      # if (t%in%names(init_segm)){ # note: t can never be in init_segm ???
      #   init_nodes <- road_netw[ID%in%init_segm[[as.character(t)]],c(FromNode,ToNode)]
      #   node_state[FromNode%in%init_nodes,state:=1]
      #   road_netw[ID%in%init_segm[[as.character(t)]],Pinv:=1]
      # }
      ind <- which(road_netw$stateFromNode>0 & road_netw$stateToNode<1) # select links with non-empty start node and non-filled end node
      road_netw[ind,newarrivals:=   1-prod(1-(stateFromNode * Pi)) ,by=ToNode] # calculate pintro for each link
      road_netw[ind,stateToNode:=1-(prod((1-stateToNode) * (1-newarrivals))),by=ToNode] # update ToNodes with old and new state

      # road_netw[ind,stateToNode:=1-(prod((1-stateToNode) * (1-   1-prod(1-(stateFromNode * Pi)) ))),by=ToNode] # calculate pintro for each link

      newstate <- unique(road_netw[ind,c("ToNode","stateToNode")],by="ToNode") # extract new state of ToNodes to update FromNodes states
      setnames(newstate,c("FromNode","newstate")) # prepare file for merge (set names and key)
      setkey(newstate,FromNode)
      road_netw <- merge(road_netw,newstate,by="FromNode",all.x=T)

      # road_netw <- newstate[road_netw] # merge road_netw and newstate to update FromNodes states
      road_netw[newstate>0,stateFromNode:=newstate] # assigne new states to FromNodes
      road_netw[,newstate:=NULL] # remove column to avoid columns with the same names


      #  node_state_sub <- node_state[state>0,] # take a subset of occupied nodes, required to speed up 'merge' below
      #
      #  nextnodes <- road_netw[FromNode%in%node_state_sub$FromNode] # identify next nodes
      #
      # nextnodes <- merge(nextnodes,node_state_sub,by="FromNode") # merge old states and next nodes
      #
      # newstate <- nextnodes[,1-prod(1-(state * Pi)),by=ToNode] # combine all probs arriving at the same node from different nodes
      #
      # node_state[.(newstate$ToNode),newarrivals:=newstate$V1] # add new state to nodes file
      # node_state[,state:=1-((1-state)*(1-newarrivals))]   # combine old and new state
      #
      # road_netw<-merge(road_netw,node_state[,1:2],by="FromNode")
      #
      # #combine all probabilities to Pinv
      # iters<-as.numeric(names(init_segm))
      # road_netw[!ID%in%as.character(unlist(init_segm[c(which(iters<t))])),Pinv:=Pe*(1-Pi)*state]
      # road_netw[,state_node:=state]
      # road_netw[,state:=NULL]

      # store results
      if (t%in%iter_save) {
        road_netw[,Pinv:=Pe*Pi] # calculate total probability for links
        road_netw[ID%in%init_segm,Pinv:=1] # assigning Pinv=1 for initial links. No effect on spread dynamics (use nodes).
        setkey(road_netw,ID)
        setkey(road_netw_details,ID)
        road_netw_out <- road_netw_details[road_netw]
        modelList[[as.character(t)]]<-road_netw_out
        if ("stateFromNode"%in%colnames(modelList[[as.character(t)]])==FALSE){
          stop ("no stateFromNode column")
        }
      }
      #update progress bar
      info <- sprintf("%d%% done", round((t/num_iter)*100))
      setTxtProgressBar(pb, t/(100)*100, label=info)
    }
  }
  close(pb)
  cat("\n Model calculation completed \n")
  print(proc.time() - tmp)
  cat("\n Output files being created in ", dir.name, "\n")

  if (export_results) {
  cat("\n Exporting final results \n")
  write.csv(x = road_netw,file = file.path(dir.name, "ModelResults.csv"),quote = F,row.names = F)
  # assign(x = "modelList",value = modelList,envir = .GlobalEnv)
  }

  roads_shp@data<-road_netw

  if (save.restart){
  restart_data<-list(roads_shp,node_state,init_segm)
  names(restart_data)<-c("roads_shp","node_state","init_segm")
  save(restart_data, file = file.path(dir.name,"restart.Rdata"))
  }

  if (makeplot) {
    cat("\n Creating maps \n")
    plotResults(list_results=modelList,shapeObj=roads_shp,save_plot=save_plot,save_dir=dir.name)
  }

  cat("\n Simulation complete \n")
  print(proc.time() - tmp)

  return(modelList)
}


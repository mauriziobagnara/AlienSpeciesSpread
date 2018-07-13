### Merge land cover files for each single link to one single file
#
# script loads all files stored in one folder and outputs a csv 
# (if necessary) and a compressed file
#######################################################################


rm(list=ls())


# folder of stored files for single links ##########################
# setwd("/home/hanno/Bioinvasion/EBAspread/Data/GetLandCoverData/Out")
setwd("/home/hanno/tmp/Out")


## load existing file names in that folder and merge them in a list
allfiles <- list.files()
alllinks <- gsub("LC_","",allfiles)
alllinks <- gsub("_50m.csv","",alllinks)

all_cats <- list()
# ptm <- proc.time()
for (i in 1:length(allfiles)){#
  dat <- read.table(allfiles[i],stringsAsFactors = F,sep=",",header=T)
  colnames(dat) <- c("LC_ID","prop")
  dat$prop <- round(dat$prop,3)

  all_cats[[i]] <- dat
  names(all_cats)[i] <- alllinks[i]
  
  if (i%%10000==0) print(i)
}
# print(proc.time()-ptm)


## convert to data.frame ########
all_cats_df <- do.call("rbind",all_cats)
rownames(all_cats_df) <- NULL

nrep <- lapply(all_cats,function(s) dim(s)[1])
linknames <- rep(names(all_cats),times=nrep)

all_cats_df$LinkID <- linknames


### Ouput ############################



write.table(all_cats_df,"../../Bioinvasion/EBAspread/Data/GetLandCoverData/Out/LandCocer_Links_50m.csv",row.names=F)
saveRDS(all_cats_df,file="../../Bioinvasion/EBAspread/Data/GetLandCoverData/Out/LandCocer_Links_50m.rds")
# dput(all_cats,file="../../Bioinvasion/EBAspread/Data/GetLandCoverData/Out/LandCocer_Links_50m.rls")

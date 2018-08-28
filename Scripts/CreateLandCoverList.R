LandCover<-readRDS(file = "data/LandCocer_Roads_50m.rds", refhook = NULL)

# for every link ID, sum proportion of LC_ID betweeen 10 and 34
ids<-as.data.frame(cbind(LinkID=unique(LandCover$LinkID),Suitable=NA),stringsAsFactors = FALSE)
LClist<-list()
for (i in 1:length(ids$LinkID)){
  LClist[[ids$LinkID[i]]]<- subset(LandCover,LinkID==ids$LinkID[i],select = c(LC_ID, prop))
  cat ("\n", i, "out of", length(ids$LinkID), "\n")
}

save(LClist,file = "data/LClist.Rdata")

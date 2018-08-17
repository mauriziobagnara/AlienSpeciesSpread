rm(list=ls())

library(RANN)

setwd("/home/hanno/Bioinvasion/EBAspread/Model")

## load node coordinates
nodeIDs <- read.table("RoadNodeLocations.csv",stringsAsFactors = F)

## location of sample
init_nodes <- matrix(c(8.364147,54.89624),nc=2)

## identify k nearest neighbours to sample site
closest <- nn2(nodeIDs[,2:3],init_nodes,  k = 5, searchtype = "radius", radius = 1)

## nearest neighbours
nodeIDs[closest[[1]],]

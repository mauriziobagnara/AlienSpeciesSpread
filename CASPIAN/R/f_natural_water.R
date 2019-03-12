# RIVER NATURAL DISPERSAL KERNEL, Elliot 2003, eq. 2a. Designed for DAILY dispersal
# a: scale parameter
# b:shape parameter
# d: distance (in Km)

f_natural_water<-function(a,b,d){
d<-d *1000 #convert Km to m
p<-a*d^(-b)
p<-p*(365/12) #calculate monthly probability
return(p)
}


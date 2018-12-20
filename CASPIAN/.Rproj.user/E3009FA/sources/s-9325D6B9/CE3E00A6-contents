# RIVER NATURAL DISPERSAL KERNEL, Radinger & Wolter 2013, eq. 1
# prop_mob: share of stationary component of the population (i.e. proportion of individuals that actively move)
# m: expected movement value. default 0
# stat: mean moving distance of stationary component. Assumed to be = flow velocity if next point is downstream, = -(flow velocity) if it's upstream
# mob: mean moving distance of mobile component. Found to be 13.67*stat (median value)


f_natural_water<-function(prop_mob,m=0,stat,mob,d){
if(stat>0){
          stat_p<-1/(sqrt(2*pi*stat^2)) * exp(-(d-m)^2/(2*stat^2)) #valid if next point is downstream
          } else if (stat==0){
            stat_p<-0   # valid if there is no current: who doesn't move actively never gets there
          } else if (stat<0){
            stat_p<- -(1/(sqrt(2*pi*stat^2)) * exp(-(d-m)^2/(2*stat^2))) #valid if next point is upstream: mob_p must overcome stat_p to proceed further
          }

mob_p<-1/(sqrt(2*pi*mob^2)) * exp(-(d-m)^2/(2*mob^2))

p<- prop_mob*mob_p + (1-prop_mob)*stat_p
p[p<0]<-0 # probability cannot be negative: if stat_p<0 & mob_p < abs(stat_p), a species never gets to the next point
return(p)
}


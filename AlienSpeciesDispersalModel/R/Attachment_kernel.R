## attachment kernel. Models dispersal due to distance between nodes .
# currently represented as model 2 in Taylor et al 2012, requires distance in km

# D: distance between nodes (km)
# p: pick-up probability:  can be improved using function based on seed characteristics (e.g. seed mass)
# a: shape parameter
# b: shape parameter
# g: shape parameter

f_attach <- function(D,a,b,g) return(exp(b*exp(a*(D^g))))

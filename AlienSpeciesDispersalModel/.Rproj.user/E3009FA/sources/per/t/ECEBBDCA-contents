## natural dispersal kernel. Models dispersal due tonatural processes between nodes.
# currently represented as negative exponential, Nathan et al.2016, originally requires D in m, this function requires Km

# D: distance between nodes (m)
# a: shape parameter , >0 , mean dispersal distance=2a

f_natural<-function(D,a) {
  D<-D*1000  #converting km to m
  return(1/(2*pi*(a^2))*exp(-D/a))
}


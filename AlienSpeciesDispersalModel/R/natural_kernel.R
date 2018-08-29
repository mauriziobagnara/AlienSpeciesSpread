## natural dispersal kernel. Models dispersal due tonatural processes between nodes.
# currently represented as negative exponential, Nathan et al.2012, originally requires D in m, this function requires Km

# D: distance between nodes (km)
# a: shape parameter , >0
# b: shape parameter , >0. b>1: thin-tailed ; b<1: fat-tailed

f_natural<-function(D,a,b) {
  D<-D*1000  #converting km to m
  #negative logistic:
  #return(1/(2*pi*(a^2))*exp(-D/a))

  #exponential power:
  return((b/(2*pi*(a^2)*gamma(2/b)))*exp(-(D^b)/(a^b))) #Gaussian for b=2, exponential for b=1
}


## airflow kernel. Models dispersal due to vehicle airflow between nodes.
# currently represented as lognormal, Von der Lippe et al. 2013, originally requires D in m, this function requires Km

# D: distance between nodes (m)
# a: scale parameter
# b: shape parameter


f_airflow<-function (D,b,a) {
  D<-D*1000 #converting km to m
 return(1/(sqrt(2*pi)*b*D)*exp(-((abs(log(D) - a))^2 / (2*b^2))))
  }

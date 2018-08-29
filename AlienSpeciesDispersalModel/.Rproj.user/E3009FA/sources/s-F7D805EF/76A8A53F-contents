## airflow kernel. Models dispersal due to vehicle airflow between nodes.
# currently represented as lognormal, Von der Lippe et al. 2013, originally requires D in m, this function requires Km

# D: distance between nodes (m)
# a: shape parameter
# b: shape parameter
# p: pick-up probability

f_airflow<-function (D,a,b) {
  D<-D*1000 #converting km to m
 return(1/(sqrt(2*pi)*a*D)*exp(-((abs(log(D) - b))^2 / (2*a^2))))
  }

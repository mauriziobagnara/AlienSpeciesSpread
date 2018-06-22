#calculates probabilities of non mutually exclusive events: p(E1) OR p(E2) OR p(E3)... OR p(En)

pUnion<-function(p)  1 - prod(1-p)

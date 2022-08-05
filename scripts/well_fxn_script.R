well_fxn <- function(u, nterms) {
  i <- seq(2,nterms, 2)
  p <- seq(3,nterms, 2)
  ui<- -1*(u^i)/(i*factorial(i))
  up<- (u^p)/(p*factorial(p))       
  wu <- -0.57721566 - log(u) + u + sum(ui) + sum(up)
  if (is.na(wu)) {
    return(0)
  } else {
  return(wu)
  }
}

list_well_fxn <- function (listu, nterms) {
  wu<-vector(mode="numeric",length=length(listu))
  j <-1
  for (i in listu) {
    wu[j]<-well_fxn(i,nterms)
    j=j+1
  }
  return(wu)
}

well_fxn(1,20) # should be .219
well_fxn(5,20) # should be .001
well_fxn(1e-10,20) # should be 22.45
well_fxn(1e-3,20) # should be 6.33

list_well_fxn(1:5,20)
list_well_fxn(c(1e-10,1e-3,1,5),20)
list_well_fxn(1:3,20)

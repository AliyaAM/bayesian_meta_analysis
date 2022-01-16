


PooledN <- function(data, Construct) {

  index = data$Construct == Construct
  TotalNforConstruct = filter(data, Construct == data[index,]$Construct)
  
  N = sum(TotalNforConstruct$N)
  
  return(params = list(N = N))
}

library(coda)

library("bayesplot")
library("ggplot2")
library("rstan") 


set.seed(15022021)
tolerance = 1e-2 
nmc = 1e+8

ContingenciesTable_MCMC = function(N, LOGOdds_Ratio){
  N = N
  LOGOdds_Ratio = LOGOdds_Ratio
  print("The N and LOgOddsRatio for this Construct is:")
  print(N)
  print(LOGOdds_Ratio)
  
  r1 = rbinom(1, N, prob = 0.5)
  N_PA_X= rbinom(1, r1, prob =0.5)
  N_noPA_X = r1 - N_PA_X
  
  r2 = rbinom(1, N, prob = 0.5)
  N_PA_noX= rbinom(1, r2, prob = 0.5)
  N_noPA_noX = r2 - N_PA_noX
  

  samples_Contingencies = array(dim = c(4, nmc),dimnames = NULL)
  samples_Contingencies[,1] = c(N_PA_X, N_noPA_X, N_PA_noX, N_noPA_noX)
  
  
  p1 = array(dim = c(1, nmc), dimnames = NULL) 
  p1[1] = c(0.5)
  p2 = array(dim = c(1, nmc), dimnames =  NULL)
  p2[1] = c(0.5)
  p3 = array(dim = c(1, nmc), dimnames = NULL) 
  p3[1] = c(0.5)
  p4 = array(dim = c(1, nmc), dimnames = NULL)
  p4[1] = c(0.5)
  
  
  for (j in 2:nmc){
    N = N
    LOGOdds_Ratio = LOGOdds_Ratio
    
    p1[j] = rnorm(n = 1, mean = p1[j-1], sd = 0.1)
    p2[j] =  rnorm(n = 1, mean = p2[j-1], sd = 0.1)
    p4[j] = rnorm(n = 1, mean = p4[j-1] , sd = 0.1)
    
    
    #r1 is the total number of those with the construct
    r1 = rbinom(1, N, prob = p1[j])
    
    #the contingency table box 1 is the number of those who exercised (PA = 1)  and p2 is the probability for PA given X = 1    ~~~~~~P(PA|X)
    samples_Contingencies[1,j] = rbinom(1, r1, prob = p2[j])
    
    
    #the contingency table box 2 is the number of those who did NOT exercise (PA = 0) and p2 is the probability for PA given X = 1 ~~~~~P(noPA|X)
    samples_Contingencies[2,j] = r1 - samples_Contingencies[1,j]
    
    
    
    #r2 is the total number of those without the construct (X = 0)
    r2 = N - r1
    
    #the contingency table box 3 is the number of those who exercised (PA = 1) and p4 is the probability for PA given X = 0 ~~~~~P(PA|noX)
    samples_Contingencies[3,j] = rbinom(1, r2,prob = p4[j])
    
    
    ##############################################################################################################################~~~~~P(noPA|noX)
    samples_Contingencies[4,j] = r2 - samples_Contingencies[3,j]
    
    #sample the frequencies 2x2 table before sampling posterior distribution (in a separate loop)
    
    sprintf("The starting values for the contingecies are", samples_Contingencies[1,j], samples_Contingencies[2,j], samples_Contingencies[3,j], samples_Contingencies[4,j])
    
    
    if((abs((samples_Contingencies[1,j] + samples_Contingencies[2,j] + samples_Contingencies[3,j] + samples_Contingencies[4,j]) - N) < tolerance)
       && (abs(log((samples_Contingencies[1,j]/samples_Contingencies[4,j])/(samples_Contingencies[2,j]/samples_Contingencies[3,j])) - LOGOdds_Ratio) < tolerance)){
      
      
      N_PA_X = samples_Contingencies[1,j]
      N_noPA_X = samples_Contingencies[2,j]
      N_PA_noX = samples_Contingencies[3,j]
      N_noPA_noX = samples_Contingencies[4,j]

      
      print(sprintf("N_PA_X:%s, N_noPA_X: %s, N_PA_noX %s, N_noPA_noX:%s",  N_PA_X, N_noPA_X, N_PA_noX, N_noPA_noX))
      
      
      return(params = list(N_PA_X = N_PA_X, N_noPA_X = N_noPA_X, N_PA_noX = N_PA_noX, N_noPA_noX = N_noPA_noX))
      #acceptance = 1-mean(duplicated(Chain[-(1:burnIn),]))
    }
    p1[j] = p1[j-1]
    p2[j] = p2[j-1]
    p4[j] = p4[j-1]
    
  }

}



source(paste(SOURCE_ROOT, "PooledN.R", sep=""))
source(paste(SOURCE_ROOT, "PooledOddsRatio_metaanalysis.R", sep=""))


N_success=function(N, Odds_Ratio) {

  #Compute the number of successes from Odds ratio 
  # N = total number of participnts in quant studies, Odds_ratio_PA_X = pooled odds ratio across quant studies, a and b are shape1 and shape2 from prior based on quali studies
  
# the below is derived from: i) the Bayes rule, ii) the fact that the probability of PA_X + noPA_X = 1; iii) N_noPA_X + N_PA_X + N_PA_noX + N_noPA_noX = N, iv) Odds ratio = [N_PA_X/N_noPA_X]/[N_PA_noX/N_noPA_noX] 

  # '(1/x) - 1 = first number in the odds ratio, while the second number in the odds ratio is 1' from this it follows that N_noPA_X = N * (1/(Odds_ratio_PA_X+1))
  
  #P(PA_X)/P(noPA_X) = Odds ratio 
  #P(PA_X)/1 - P(PA_X) = Odds ratio
  #P(PA_X)/Odds Ratio = 1 - P(PA_X)
  #P(PA_X) = Odds ratio - Odds RAtio*(P(PA_X)) 
  
  
  
  N_PA_X = N * (1/(Odds_Ratio+1))
  N_noPA_X = N - N * (1/(Odds_Ratio+1))
  
  
  
  return(params = list(N_PA_X = N_PA_X, N_noPA_X = N_noPA_X))

}
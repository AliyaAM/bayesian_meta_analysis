

#source the Bayesian meta-analysis that included both qualitative and quantitative evidence  starting at 10 different seeds 
#seeds are used to account for variations in MCMC sampling 
#the final results are a summary of 10 seeds averaged 

Source_seed = function(uncertainty, seed) {
  
  source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/BayesianMetaAnalysis_StepByStep.R', local = TRUE)
  
  MAP_uncertainty_seed_tag = data.frame(Pooled_LOGOdds_Ratio_posterior_string_SEED,
                                        LowerCI_LogOddsRatio_posterior_string_SEED, 
                                        UpperCI_LogOddsRatio_posterior_string_SEED,
                                        posterior_alpha_Ratio_posterior_string_SEED,
                                        posterior_beta_Ratio_posterior_string_SEED, 
                                        mean_posterior_string_SEED, 
                                        posterior_CredibleInterval_0.05_string_SEED, 
                                        posterior_CredibleInterval_0.95_string_SEED, 
                                        uncertainty, 
                                        seed)

  names(MAP_uncertainty_seed_tag) <- c("LogOR", 
                                       "LogOR_CI_lower",
                                       "LogOR_CI_upper",
                                       "posterior_alpha", 
                                       "posterior_beta",
                                       "MAP", 
                                       "posterior_CredibleInterval_0.05", 
                                       "posterior_CredibleInterval_0.95", 
                                       "uncertainty", 
                                       "seed")
  
  #The data is sourced within BayesianMetaAnalysis_StepByStep.R function 
  #below is the list of constructs that were indentified within qualitative and quantitative evidence (Construct_name)

  Construct_name = c("Age",
                     "Comorbidity",
                     "SocialSupport",
                     "NegativeAttitute",
                     "PositiveAttitute",
                     "6MWT",
                     "Functioning",
                     "Symptoms",
                     "LVEF",
                     "SelfEfficacy")
  
  MAP_uncertainty_seed_tag = cbind(MAP_uncertainty_seed_tag, Construct_name)
  
  return(MAP_uncertainty_seed_tag)   
  
}

#uncertainty level is set to 10. This choice does not change the final results due to narrow space from which distributions are sampled within BayesianMetaAnalysis_StepByStep.R function 
#this was tested in a sensetivity analysis comparing the results of two analyses 1) where uncertainty was set to 10; 2) where uncertainty was set to 0.1 

uncertaintyLevels_MAP = Source_seed(uncertainty = 10, seed = 888100)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888101)

uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888102)
uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888103)

uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888104)
uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888105)

uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888106)
uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888107)

uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888108)
uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888110)

uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888111)
uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

#save the results of the Bayesian meta-analysis run from 10 different seeds below: 
file_uncertaintyLevels_MAP <- file.path("/Users/aliya/my_docs/proj/bayesian_meta_analysis/seeds_MAP_23Feb2021.csv")
fn_uncertaintyLevels_MAP <- as.filename(file_uncertaintyLevels_MAP)
make_path(fn_uncertaintyLevels_MAP)
write.table(uncertaintyLevels_MAP, file = file_uncertaintyLevels_MAP, 
            append = FALSE, 
            quote = TRUE, 
            sep = ",", 
            eol = "\r", 
            na = "NA", 
            dec = ".",
            row.names = FALSE, 
            col.names = TRUE, 
            fileEncoding = "" )


#save all plots produced by the BayesianMetaAnalysis_StepByStep.R function below: 

All_plot = ggplot(data = uncertaintyLevels_MAP, aes(seed, MAP)) + 
  geom_point(aes(colour = factor(Construct_name)), size = 4)

print(uncertaintyLevels_MAP)

Age_uncertaintyLevels_MAP_plot = ggplot(data = uncertaintyLevels_MAP, aes(seed, MAP)) + 
  geom_point(aes(colour = factor()), size = 4)


Comorbidity_uncertaintyLevels_MAP = ggplot(data = uncertaintyLevels_MAP, aes(seed, MAP)) + 
  geom_point()

SocialSupport_uncertaintyLevels_MAP = ggplot(data = uncertaintyLevels_MAP, aes(seed, MAP)) + 
  geom_point()

NegativeAttitute_uncertaintyLevels_MAP = ggplot(data = uncertaintyLevels_MAP, aes(seed, MAP)) + 
  geom_point()

PositiveAttitute_uncertaintyLevels_MAP = ggplot(data = uncertaintyLevels_MAP, aes(seed, MAP)) + 
  geom_point()

SixMWT_uncertaintyLevels_MAP = ggplot(data = uncertaintyLevels_MAP, aes(seed,  MAP)) + 
  geom_point()

Functioning_uncertaintyLevels_MAP = ggplot(data = uncertaintyLevels_MAP, aes(seed, MAP)) + 
  geom_point()

Symptoms_uncertaintyLevels_MAP = ggplot(data = uncertaintyLevels_MAP, aes(seed, MAP)) + 
  geom_point()

LVEF_uncertaintyLevels_MAP = ggplot(data = uncertaintyLevels_MAP, aes(seed, MAP)) + 
  geom_point()

SelfEfficacy_uncertaintyLevels_MAP = ggplot(data = uncertaintyLevels_MAP, aes(seed, MAP)) + 
  geom_point()

print(Comorbidity_uncertaintyLevels_MAP)
print(SocialSupport_uncertaintyLevels_MAP)
print(NegativeAttitute_uncertaintyLevels_MAP)
print(PositiveAttitute_uncertaintyLevels_MAP)
print(SixMWT_uncertaintyLevels_MAP)
print(Functioning_uncertaintyLevels_MAP)
print(Symptoms_uncertaintyLevels_MAP)
print(LVEF_uncertaintyLevels_MAP)
print(SelfEfficacy_uncertaintyLevels_MAP)




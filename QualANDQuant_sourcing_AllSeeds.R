

#source the Bayesian meta-analysis that included both qualitative and quantitative evidence  starting at 10 different seeds 
#seeds are used to account for variations in MCMC sampling 
#the final results are a summary of 10 seeds averaged 

library(filenamer) # library for as.filename

Source_seed = function(uncertainty, seed) {
  
  source(paste(SOURCE_ROOT, "BayesianMetaAnalysis_StepByStep.R", sep=""), local = TRUE)

  MAPQualQuant_uncertainty_seed_tag = data.frame(Pooled_LOGOdds_Ratio_posterior_string_SEED,
                                        LowerCI_LogOddsRatio_posterior_string_SEED, 
                                        UpperCI_LogOddsRatio_posterior_string_SEED,
                                        posterior_alpha_Ratio_posterior_string_SEED,
                                        posterior_beta_Ratio_posterior_string_SEED, 
                                        mean_posterior_string_SEED, 
                                        posterior_CredibleInterval_0.05_string_SEED, 
                                        posterior_CredibleInterval_0.95_string_SEED, 
                                        uncertainty, 
                                        seed)

  names(MAPQualQuant_uncertainty_seed_tag) <- c("LogOR", 
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
  
  MAPQualQuant_uncertainty_seed_tag = cbind(MAPQualQuant_uncertainty_seed_tag, Construct_name)
  
  return(MAPQualQuant_uncertainty_seed_tag)   
  
}

#uncertainty level is set to 10. This choice does not change the final results due to narrow space from which distributions are sampled within BayesianMetaAnalysis_StepByStep.R function 
#this was tested in a sensetivity analysis comparing the results of two analyses 1) where uncertainty was set to 10; 2) where uncertainty was set to 0.1 

uncertaintyLevels_MAPQualQuant = Source_seed(uncertainty = 10, seed = 888100)

MAPQualQuant_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888101)

uncertaintyLevels_MAPQualQuant = rbind(uncertaintyLevels_MAPQualQuant, MAPQualQuant_uncertainty_seed_tag)

MAPQualQuant_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888102)
uncertaintyLevels_MAPQualQuant = rbind(uncertaintyLevels_MAPQualQuant, MAPQualQuant_uncertainty_seed_tag)

MAPQualQuant_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888103)

uncertaintyLevels_MAPQualQuant = rbind(uncertaintyLevels_MAPQualQuant, MAPQualQuant_uncertainty_seed_tag)

MAPQualQuant_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888104)
uncertaintyLevels_MAPQualQuant = rbind(uncertaintyLevels_MAPQualQuant, MAPQualQuant_uncertainty_seed_tag)

MAPQualQuant_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888105)

uncertaintyLevels_MAPQualQuant = rbind(uncertaintyLevels_MAPQualQuant, MAPQualQuant_uncertainty_seed_tag)

MAPQualQuant_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888106)
uncertaintyLevels_MAPQualQuant = rbind(uncertaintyLevels_MAPQualQuant, MAPQualQuant_uncertainty_seed_tag)

MAPQualQuant_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888107)

uncertaintyLevels_MAPQualQuant = rbind(uncertaintyLevels_MAPQualQuant, MAPQualQuant_uncertainty_seed_tag)

MAPQualQuant_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888108)
uncertaintyLevels_MAPQualQuant = rbind(uncertaintyLevels_MAPQualQuant, MAPQualQuant_uncertainty_seed_tag)

MAPQualQuant_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888110)

uncertaintyLevels_MAPQualQuant = rbind(uncertaintyLevels_MAPQualQuant, MAPQualQuant_uncertainty_seed_tag)

MAPQualQuant_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888111)
uncertaintyLevels_MAPQualQuant = rbind(uncertaintyLevels_MAPQualQuant, MAPQualQuant_uncertainty_seed_tag)

#save the results of the Bayesian meta-analysis of the qualitative combined with quantitative evidence that was run from 10 different seeds below: 
file_uncertaintyLevels_MAPQualQuant <- file.path(paste(OUTPUT_ROOT, "seeds_MAPQualQuant.csv", sep=""))

fn_uncertaintyLevels_MAPQualQuant <- as.filename(file_uncertaintyLevels_MAPQualQuant)
make_path(fn_uncertaintyLevels_MAPQualQuant)
write.table(uncertaintyLevels_MAPQualQuant, file = file_uncertaintyLevels_MAPQualQuant, 
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

All_plot = ggplot(data = uncertaintyLevels_MAPQualQuant, aes(seed, MAP)) + 
  geom_point(aes(colour = factor(Construct_name)), size = 4)

print(uncertaintyLevels_MAPQualQuant)

Age_uncertaintyLevels_MAPQualQuant_plot = ggplot(data = uncertaintyLevels_MAPQualQuant, aes(seed, MAP)) + 
  geom_point(aes(colour = factor()), size = 4)


Comorbidity_uncertaintyLevels_MAPQualQuant = ggplot(data = uncertaintyLevels_MAPQualQuant, aes(seed, MAP)) + 
  geom_point()

SocialSupport_uncertaintyLevels_MAPQualQuant = ggplot(data = uncertaintyLevels_MAPQualQuant, aes(seed, MAP)) + 
  geom_point()

NegativeAttitute_uncertaintyLevels_MAPQualQuant = ggplot(data = uncertaintyLevels_MAPQualQuant, aes(seed, MAP)) + 
  geom_point()

PositiveAttitute_uncertaintyLevels_MAPQualQuant = ggplot(data = uncertaintyLevels_MAPQualQuant, aes(seed, MAP)) + 
  geom_point()

SixMWT_uncertaintyLevels_MAPQualQuant = ggplot(data = uncertaintyLevels_MAPQualQuant, aes(seed,  MAP)) + 
  geom_point()

Functioning_uncertaintyLevels_MAPQualQuant = ggplot(data = uncertaintyLevels_MAPQualQuant, aes(seed, MAP)) + 
  geom_point()

Symptoms_uncertaintyLevels_MAPQualQuant = ggplot(data = uncertaintyLevels_MAPQualQuant, aes(seed, MAP)) + 
  geom_point()

LVEF_uncertaintyLevels_MAPQualQuant = ggplot(data = uncertaintyLevels_MAPQualQuant, aes(seed, MAP)) + 
  geom_point()

SelfEfficacy_uncertaintyLevels_MAPQualQuant = ggplot(data = uncertaintyLevels_MAPQualQuant, aes(seed, MAP)) + 
  geom_point()

print(Comorbidity_uncertaintyLevels_MAPQualQuant)
print(SocialSupport_uncertaintyLevels_MAPQualQuant)
print(NegativeAttitute_uncertaintyLevels_MAPQualQuant)
print(PositiveAttitute_uncertaintyLevels_MAPQualQuant)
print(SixMWT_uncertaintyLevels_MAPQualQuant)
print(Functioning_uncertaintyLevels_MAPQualQuant)
print(Symptoms_uncertaintyLevels_MAPQualQuant)
print(LVEF_uncertaintyLevels_MAPQualQuant)
print(SelfEfficacy_uncertaintyLevels_MAPQualQuant)




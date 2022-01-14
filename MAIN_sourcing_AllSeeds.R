#source the Bayesian meta-analysis (full) over different seeds 
Source_seed = function(uncertainty, seed) {
  
  source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/BayesianMetaAnalysis_StepByStep.R', local = TRUE)
  
  print("COMPLETED inside the function!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  
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
  print("made df df df df df df df df df df df")
  
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
  print("after naming")
  
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


uncertaintyLevels_MAP = Source_seed(uncertainty = 10, seed = 888100)

print("done with function")

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


print("rbinded the second fucntion")

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

print(" exported uncertaintyLevels_MAP file")

All_plot = ggplot(data = uncertaintyLevels_MAP, aes(seed, MAP)) + 
  geom_point(aes(colour = factor(Construct_name)), size = 4)

print(uncertaintyLevels_MAP)

print("made the first plot")


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

#save plots in a new folder
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="/Users/aliya/my_docs/proj/bayesian_meta_analysis/seed_MAP_PLOTS")
print(uncertaintyLevels_MAP)


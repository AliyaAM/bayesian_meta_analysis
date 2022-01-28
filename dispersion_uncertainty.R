library(dplyr)

## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"

uncertainty_in_the_evidence = read.csv("/Users/aliya/my_docs/THESIS/First_Draft01March2020/BayesianMetaAnalysis/Version_revision2022/variance_hyperprior_likelihood_posterior.csv") 

unique(uncertainty_in_the_evidence$SD_prior_x)
unique(uncertainty_in_the_evidence$SD_quant_x)
unique(uncertainty_in_the_evidence$SD_posterior_x)

#1 - narrow dispersion ~ low uncerrtainty 
# 2, 3, 4 = medium dispersion ~ moderate uncertainty 
#5, 6, 7, 8 wide dispersion ~ high uncertainty 


uncertainty_prior = case_when(uncertainty_in_the_evidence$SD_prior_x == 1 ~ "narrow/low", 
                              uncertainty_in_the_evidence$SD_prior_x == 2 ~ "medium/moderate", 
                              uncertainty_in_the_evidence$SD_prior_x == 3 ~ "medium/moderate",
                              uncertainty_in_the_evidence$SD_prior_x == 4 ~ "medium/moderate",
                              uncertainty_in_the_evidence$SD_prior_x == 5 ~ "wide/high",
                              uncertainty_in_the_evidence$SD_prior_x == 6 ~ "wide/high",
                              uncertainty_in_the_evidence$SD_prior_x == 7 ~ "wide/high",
                              uncertainty_in_the_evidence$SD_prior_x == 8 ~ "wide/high")
                              
uncertainty_in_the_evidence$uncertainty_prior  = uncertainty_prior                     

uncertainty_quant = case_when(uncertainty_in_the_evidence$SD_quant_x == 1 ~ "narrow/low", 
                              uncertainty_in_the_evidence$SD_quant_x == 2 ~ "medium/moderate", 
                              uncertainty_in_the_evidence$SD_quant_x == 3 ~ "medium/moderate",
                              uncertainty_in_the_evidence$SD_quant_x == 4 ~ "medium/moderate",
                              uncertainty_in_the_evidence$SD_quant_x == 5 ~ "wide/high",
                              uncertainty_in_the_evidence$SD_quant_x == 6 ~ "wide/high",
                              uncertainty_in_the_evidence$SD_quant_x == 7 ~ "wide/high",
                              uncertainty_in_the_evidence$SD_quant_x == 8 ~ "wide/high")

uncertainty_in_the_evidence$uncertainty_quant = uncertainty_quant


uncertainty_posterior = case_when(uncertainty_in_the_evidence$SD_posterior_x == 1 ~ "narrow/low", 
                              uncertainty_in_the_evidence$SD_posterior_x == 2 ~ "medium/moderate", 
                              uncertainty_in_the_evidence$SD_posterior_x == 3 ~ "medium/moderate",
                              uncertainty_in_the_evidence$SD_posterior_x == 4 ~ "medium/moderate",
                              uncertainty_in_the_evidence$SD_posterior_x == 5 ~ "wide/high",
                              uncertainty_in_the_evidence$SD_posterior_x == 6 ~ "wide/high",
                              uncertainty_in_the_evidence$SD_posterior_x == 7 ~ "wide/high",
                              uncertainty_in_the_evidence$SD_posterior_x == 8 ~ "wide/high")

uncertainty_in_the_evidence$uncertainty_posterior =uncertainty_posterior

write.table(uncertainty_in_the_evidence, file = paste(OUTPUT_ROOT, "uncertainty_in_the_evidence.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )

narrow_low_uncertainty_posterior = filter(uncertainty_in_the_evidence, uncertainty_in_the_evidence$uncertainty_posterior == "narrow/low")
medium_moderate_uncertainty_posterior = filter(uncertainty_in_the_evidence, uncertainty_in_the_evidence$uncertainty_posterior == "medium/moderate")
wide_high_uncertainty_posterior = filter(uncertainty_in_the_evidence, uncertainty_in_the_evidence$uncertainty_posterior == "wide/high")


narrow_low_uncertainty_quant = filter(uncertainty_in_the_evidence, uncertainty_in_the_evidence$uncertainty_quant == "narrow/low")
medium_moderate_uncertainty_quant = filter(uncertainty_in_the_evidence, uncertainty_in_the_evidence$uncertainty_quant == "medium/moderate")
wide_high_uncertainty_quant = filter(uncertainty_in_the_evidence, uncertainty_in_the_evidence$uncertainty_quant == "wide/high")

results_ordered = rbind(narrow_low_uncertainty_quant, medium_moderate_uncertainty_quant, wide_high_uncertainty_quant)

narrow_low_uncertainty_prior = filter(uncertainty_in_the_evidence, uncertainty_in_the_evidence$uncertainty_prior == "narrow/low")
medium_moderate_uncertainty_prior = filter(uncertainty_in_the_evidence, uncertainty_in_the_evidence$uncertainty_prior == "medium/moderate")
wide_high_uncertainty_prior = filter(uncertainty_in_the_evidence, uncertainty_in_the_evidence$uncertainty_prior == "wide/high")


write.table(results_ordered, file = paste(OUTPUT_ROOT, "results_ordered_by_uncertainty.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )
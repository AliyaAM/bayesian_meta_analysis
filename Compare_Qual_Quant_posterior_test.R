
source(paste(SOURCE_ROOT, "BayesUpdateStepByStep.R", sep=""), local = TRUE) # this function (BayesUpdateStepByStep) runs the Bayesian meta-analysis that combines qualitative and quantitative evidence 


Results_Age =   BayesUpdateStepByStep(x =x, Construct = "Age")
Results_BayesianMeta_Analysis = rbind(Results_Age)


Results_sixMWT =   BayesUpdateStepByStep(x =x, Construct = "6MWT")
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_sixMWT)

Results_Symptoms =   BayesUpdateStepByStep(x =x, Construct = "Symptoms"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_Symptoms)


Results_LVEF =   BayesUpdateStepByStep(x =x, Construct = "LVEF"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_LVEF)

Results_SelfEfficacy =   BayesUpdateStepByStep(x =x, Construct = "SelfEfficacy"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_SelfEfficacy)

Results_SocialSupport =   BayesUpdateStepByStep(x =x, Construct = "SocialSupport"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_SocialSupport)

Results_Comorbidity =   BayesUpdateStepByStep(x =x, Construct = "Comorbidity"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_Comorbidity) 

Results_NegativeAttitude =   BayesUpdateStepByStep(x =x, Construct = "NegativeAttitude"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_NegativeAttitude)

Results_Functioning =   BayesUpdateStepByStep(x =x, Construct = "Functioning"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_Functioning)

Results_PositiveAttitude =   BayesUpdateStepByStep(x =x, Construct = "PositiveAttitude"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_PositiveAttitude)

head(Results_BayesianMeta_Analysis)

write.table(Results_BayesianMeta_Analysis, file = paste(OUTPUT_ROOT, "Results_BayesianMeta_Analysis_data_qual_quant.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )




density_by_Construct = function(data, Construct){
  index = data$Construct == Construct
  logOddsRatio = seq( -2, 3 , length=1000)
  filtered_data = filter(data, Construct == data[index,]$Construct)
  
  # the results of the expert elicitation task 
  Prior_qual_density = dnorm(logOddsRatio,
                             filtered_data$logOR_expert_elicitation_task, 
                             filtered_data$variance_expert_elicitation_task)
  
  # posterior resulted from updating hyperprior with the results of the expert elicitaiton task 
  Posterior_qual_only = dnorm(logOddsRatio, 
                              filtered_data$Posterior_qual_only_mean,  
                              filtered_data$Posterior_qual_only_variance)
  
  # likelihood (quantitative evidence only)
  Likelihood = dnorm(logOddsRatio, 
                     filtered_data$LOGOdds_Ratio_quant, 
                     filtered_data$variance_quant)
  
  
  # the posterior resulted from updating prior with likelihood 
  posterior_QualplusQuant = dnorm(logOddsRatio, 
                                  filtered_data$posterior_QualplusQuant_mean,
                                  filtered_data$posterior_QualplusQuant_variance)
  
  # the posterior resulted from updating hyperprior with prior and then with likelihood 
  posterior_All = dnorm(logOddsRatio, 
                        filtered_data$posterior_All_mean,
                        filtered_data$posterior_All_variance)
  
  df = data.frame(logOddsRatio, Construct, Prior_qual_density, Posterior_qual_only, Likelihood, posterior_QualplusQuant, posterior_All)
  colnames(df) = c("logOddsRatio", "Construct", "Prior_qual_density", "Posterior_qual_only", "Likelihood",  "posterior_QualplusQuant", "posterior_All")
  return(df)
}

Age_density_by_Construct = density_by_Construct(data = Results_BayesianMeta_Analysis, Construct = "Age")
Comorbidity_density_by_Construct = density_by_Construct(data = Results_BayesianMeta_Analysis, Construct = "Comorbidity")
SocialSupport_density_by_Construct = density_by_Construct(data = Results_BayesianMeta_Analysis, Construct = "SocialSupport")
NegativeAttitude_density_by_Construct = density_by_Construct(data = Results_BayesianMeta_Analysis, Construct = "NegativeAttitude")
PositiveAttitude_density_by_Construct = density_by_Construct(data = Results_BayesianMeta_Analysis, Construct = "PositiveAttitude")
SixMWT_density_by_Construct = density_by_Construct(data = Results_BayesianMeta_Analysis, Construct = "6MWT")
Functioning_density_by_Construct = density_by_Construct(data = Results_BayesianMeta_Analysis, Construct = "Functioning")
Symptoms_density_by_Construct = density_by_Construct(data = Results_BayesianMeta_Analysis, Construct = "Symptoms")
LVEF_density_by_Construct = density_by_Construct(data = Results_BayesianMeta_Analysis, Construct = "LVEF")
SelfEfficacy_density_by_Construct = density_by_Construct(data = Results_BayesianMeta_Analysis, Construct = "SelfEfficacy")


height = c(rep(10, 1000),
           rep(20, 1000), 
           rep(30, 1000), 
           rep(40, 1000), 
           rep(50, 1000), 
           rep(60, 1000), 
           rep(70, 1000), 
           rep(80, 1000), 
           rep(90, 1000),
           rep(100, 1000))

length(height)
density_ALL_Construct = rbind(Age_density_by_Construct,
                              Comorbidity_density_by_Construct,
                              SocialSupport_density_by_Construct,
                              NegativeAttitude_density_by_Construct,
                              PositiveAttitude_density_by_Construct,
                              SixMWT_density_by_Construct, 
                              Functioning_density_by_Construct,
                              Symptoms_density_by_Construct,
                              LVEF_density_by_Construct, 
                              SelfEfficacy_density_by_Construct)

density_ALL_Construct = cbind(density_ALL_Construct, height)
nrow(density_ALL_Construct)


#d <- data.frame(
#  x = density_ALL_Construct$logOddsRatio, 
#  y = c(Age_density_by_Construct$posterior_All,
#        Comorbidity_density_by_Construct$posterior_All,
#        SocialSupport_density_by_Construct$posterior_All,
#        NegativeAttitude_density_by_Construct$posterior_All,
#        PositiveAttitude_density_by_Construct$posterior_All,
#        SixMWT_density_by_Construct$posterior_All, 
#        Functioning_density_by_Construct$posterior_All,
#        Symptoms_density_by_Construct$posterior_All,
#        LVEF_density_by_Construct$posterior_All, 
#        SelfEfficacy_density_by_Construct$posterior_All),
#  height = height)

#plot5 = ggplot(d, aes(x, y, height = height, group = y)) + 
#  geom_ridgeline(fill = "lightblue")

#print(plot5)


#plotting the results of the expert elicitation task 
Plot_Prior_qual_density = ggplot(density_ALL_Construct, aes(x = logOddsRatio, y = Construct, height=Prior_qual_density, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-2, 3 )

print(Plot_Prior_qual_density)

#plotting posterior resulted from updating hyperprior with the results of the expert elicitaiton task 
#Plot_Posterior_qual_only = ggplot(density_ALL_Construct, aes(x = logOddsRatio, y = Construct, height=Posterior_qual_only, group = Construct)) +
#  geom_density_ridges(stat = "identity", scale = 1) +
# xlim(-2, 2.5  )

#print(Plot_Posterior_qual_only)

#plotting likelihood (quantitative evidence only)
Plot_Likelihood = ggplot(density_ALL_Construct, aes(x = logOddsRatio, y = Construct, height=Likelihood, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-2, 3  )

print(Plot_Likelihood)

#plotting the posterior resulted from updating prior with likelihood 
Plot_posterior_QualplusQuant = ggplot(density_ALL_Construct, aes(x = logOddsRatio, y = Construct, height=posterior_QualplusQuant, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-2, 3  )

print(Plot_posterior_QualplusQuant)

#plotting the posterior resulted from updating hyperprior with prior and then with likelihood 
#Plot_posterior_All = ggplot(density_ALL_Construct, aes(x = logOddsRatio, y = Construct, height=posterior_All, group = Construct)) +
#  geom_density_ridges(stat = "identity", scale = 1) +
#  xlim(-2, 2.5 )

#print(Plot_posterior_All)


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)


x_directory_quant <- file.path(paste(OUTPUT_ROOT, "/PLOTS_QUANT_and_qual", sep=""))
dir.create(x_directory_quant)
file.copy(from=plots.png.paths, to=x_directory_quant)

########################################################################################################
########################################################################################################

#code for plotting quantitative evidence (prior), next to likelihood, and the posterior. 

#in the below table ombine Prior_qual_density, Likelihood, posterior_QualplusQuant

d <- data.frame(
  x = density_ALL_Construct$logOddsRatio, 
  y = c(Age_density_by_Construct$Prior_qual_density, 
        Age_density_by_Construct$Likelihood, 
        Age_density_by_Construct$posterior_QualplusQuant, 
  
        
        Comorbidity_density_by_Construct$Prior_qual_density,
        Comorbidity_density_by_Construct$Likelihood,
        Comorbidity_density_by_Construct$posterior_QualplusQuant,
        
        
        SocialSupport_density_by_Construct$Prior_qual_density,
        SocialSupport_density_by_Construct$Likelihood,
        SocialSupport_density_by_Construct$posterior_QualplusQuant,
        
        
        NegativeAttitude_density_by_Construct$Prior_qual_density,
        NegativeAttitude_density_by_Construct$Likelihood,
        NegativeAttitude_density_by_Construct$posterior_QualplusQuant,
       
        
        PositiveAttitude_density_by_Construct$Prior_qual_density,
        PositiveAttitude_density_by_Construct$Likelihood,
        PositiveAttitude_density_by_Construct$posterior_QualplusQuant,
        
        SixMWT_density_by_Construct$Prior_qual_density,
        SixMWT_density_by_Construct$Likelihood,
        SixMWT_density_by_Construct$posterior_QualplusQuant,
        
 
        Functioning_density_by_Construct$Prior_qual_density,
        Functioning_density_by_Construct$Likelihood,
        Functioning_density_by_Construct$posterior_QualplusQuant,
        

        Symptoms_density_by_Construct$Prior_qual_density,
        Symptoms_density_by_Construct$Likelihood,
        Symptoms_density_by_Construct$posterior_QualplusQuant,
        
        
        LVEF_density_by_Construct$Prior_qual_density,
        LVEF_density_by_Construct$Likelihood,
        LVEF_density_by_Construct$posterior_QualplusQuant,

        SelfEfficacy_density_by_Construct$Prior_qual_density,
        SelfEfficacy_density_by_Construct$Likelihood,
        SelfEfficacy_density_by_Construct$posterior_QualplusQuant),
  
  height = height)

#plot5 = ggplot(d, aes(x, y, height = height, group = y)) + 
#  geom_ridgeline(fill = "lightblue")

#print(plot5)

plot4 = ggplot(density_ALL_Construct, aes(x = logOddsRatio, 
                                          y = Construct,
                                          height= Likelihood, 
                                          group = Construct, 
                                          color = type_evidence,
                                          fill = type_evidence)) +
  
  geom_density_ridges(stat = "identity",
                      scale = 5) +
  
  xlim(0, 0.5) +
  
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.grid.major = element_line(colour = "grey", size = 0.2),
        panel.grid.minor = element_line(colour = "grey", size = 0.1)) 
#+
#scale_fill_cyclical(labels = c("Age" = "Age_qual_QUANT", 
#                               "Comorbidity"="Comorbidity_qual_QUANT", 
#                               "SocialSupport"="SocialSupport_qual_QUANT",
#                               "NegativeAttitude" ="NegativeAttitude_qual_QUANT", 
#                               "PositiveAttitude" = "PositiveAttitude_qual_QUANT",
#                               "6MWT"="6MWT_qual_QUANT",
#                               "Functioning"="Functioning_qual_QUANT",
#                               "Symptoms"= "Symptoms_qual_QUANT",
#                               "LVEF"="LVEF_qual_QUANT",
#                               "SelfEfficacy"="SelfEfficacy_qual_QUANT"), 

#                    values = c("#ff0000", "#0000ff", "#ff8080", "#8080ff"))

# +

# ylim(c("Age",
#"Comorbidity",
#"SocialSupport",
#"NegativeAttitude",
#"PositiveAttitude",
#"6MWT",
#"Functioning",
#"Symptoms",
#"LVEF",
#"SelfEfficacy",
#"BloodPressure",
#B"BMI",
#"CRT",
#"Depression",
#"Digoxin",
#"Doppler",
#"Dysphoria",
# "Education",
#"Employment",
#"Ethnicity",
# "Functioning",
# "HFDuration",
# "HFrEF_Yes",
# "highproBNP",
#"Hostility",
# "Income",
#"LAV",
# "LVAD",
# "LVR",
#"Partner",
# "PeakVO2",
# "PercievedExersion",
#"QoL",
#"RenalFunction",
#"Smoking",
# "Symptoms distress"))


print(plot4)

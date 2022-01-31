

# Set the root directory to look for source code.
SOURCE_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"


source(paste(SOURCE_ROOT, "BayesUpdateStepByStep.R", sep=""), local = TRUE) # this function (BayesUpdateStepByStep) runs the Bayesian meta-analysis that combines qualitative and quantitative evidence 

source(paste(SOURCE_ROOT, "Summary_stats_table_qual_and_quant.R", sep=""), local = TRUE) # this function (BayesUpdateStepByStep) runs the Bayesian meta-analysis that combines qualitative and quantitative evidence, and outputs the summary stats only 




x = read.csv(paste(SOURCE_ROOT, "input.csv", sep="")) #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
data = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from  the quantitative studies, the file lists all data including the data that was not used for the meta-analysis. the data not included in the meta-anslysis is for the cases when insufficient data was reported in the article for it to be pooled in the meta-analysis (for example mean but no SD or variance etc)
JaarsmaInternationalStudy = read.csv(paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")) #data used for eliciting the hyperprior (general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013)

Steps_day_data = subset(data, data$PA_Varme == "Steps/d_total")
PA_Varme = "Steps_day"



unique(Steps_day_data$Construct)
Results_Steps_day_qual_quant = data.frame()
unique(Steps_day_data$PA_Varme)
#social support, 
#Age, 6MWT,LVEF,Comorbidity1,
#symptoms, selfefficacy, negative attitude, positive attitude, physical functioning 



Steps_day_Age = BayesUpdateStepByStep(x = x, Construct = "Age")
Results_Steps_day_qual_quant = rbind(Results_Steps_day_qual_quant, Steps_day_Age)
Steps_day_6MWT = BayesUpdateStepByStep(x = x, Construct = "6MWT")
Results_Steps_day_qual_quant = rbind(Results_Steps_day_qual_quant, Steps_day_6MWT)


Steps_day_LVEF = BayesUpdateStepByStep(x = x, Construct = "LVEF")
Results_Steps_day_qual_quant = rbind(Results_Steps_day_qual_quant, Steps_day_LVEF)



Steps_day_SelfEfficacy = BayesUpdateStepByStep(x = x, Construct = "SelfEfficacy")
Results_Steps_day_qual_quant = rbind(Results_Steps_day_qual_quant, Steps_day_SelfEfficacy)


Steps_day_PhysicalFunctioning7 = BayesUpdateStepByStep(x = x, Construct = "PhysicalFunctioning")
Results_Steps_day_qual_quant = rbind(Results_Steps_day_qual_quant, Steps_day_PhysicalFunctioning7)








Summary_stats_table_qual_and_quantResults_Steps_day_qual_quant = data.frame()
Summary_stats_table_qual_and_quantSteps_day_Age = Summary_stats_table_qual_and_quant(x = x, Construct = "Age")
Summary_stats_table_qual_and_quantResults_Steps_day_qual_quant = rbind(Summary_stats_table_qual_and_quantResults_Steps_day_qual_quant, Summary_stats_table_qual_and_quantSteps_day_Age)


Summary_stats_table_qual_and_quantSteps_day_6MWT = Summary_stats_table_qual_and_quant(x = x, Construct = "6MWT")
Summary_stats_table_qual_and_quantResults_Steps_day_qual_quant = rbind(Summary_stats_table_qual_and_quantResults_Steps_day_qual_quant, Summary_stats_table_qual_and_quantSteps_day_6MWT)

Summary_stats_table_qual_and_quantSteps_day_LVEF = Summary_stats_table_qual_and_quant(x = x, Construct = "LVEF")
Summary_stats_table_qual_and_quantResults_Steps_day_qual_quant = rbind(Summary_stats_table_qual_and_quantResults_Steps_day_qual_quant, Summary_stats_table_qual_and_quantSteps_day_LVEF)



Summary_stats_table_qual_and_quantSteps_day_PhysicalFunctioning7  = Summary_stats_table_qual_and_quant(x = x, Construct = "PhysicalFunctioning")
Summary_stats_table_qual_and_quantResults_Steps_day_qual_quant = rbind(Summary_stats_table_qual_and_quantResults_Steps_day_qual_quant, Summary_stats_table_qual_and_quantSteps_day_PhysicalFunctioning7)


Summary_stats_table_qual_and_quantSteps_day_SelfEfficacy  = Summary_stats_table_qual_and_quant(x = x, Construct = "SelfEfficacy")
Summary_stats_table_qual_and_quantResults_Steps_day_qual_quant = rbind(Summary_stats_table_qual_and_quantResults_Steps_day_qual_quant, Summary_stats_table_qual_and_quantSteps_day_SelfEfficacy)



write.table(Results_Steps_day_qual_quant, file = paste(OUTPUT_ROOT, "Results_Steps_day_qual_quant.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



write.table(Summary_stats_table_qual_and_quantResults_Steps_day_qual_quant, file = paste(OUTPUT_ROOT, "Summary_stats_table_qual_and_quantResults_Steps_day_qual_quant.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



density_by_Construct_stratified = function(data, Construct){
  index = data$Construct == Construct
  logOddsRatio = seq(-3, 4  , length=1000)
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
data = Results_Steps_day_qual_quant



Age_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Age")
SixMWT_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "6MWT")
LVEF_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "LVEF")


SelfEfficacy_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "SelfEfficacy")

PhysicalFunctioning7_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "PhysicalFunctioning")



height = c(rep(1, 1000),
           rep(2, 1000), 
           rep(3, 1000), 
           rep(4, 1000),
           rep(5, 1000))

length(height)
density_ALL_Construct_quant_stratified = rbind(Age_density_by_Construct_stratified,
                                               SixMWT_density_by_Construct_stratified, 
                                              
                                               LVEF_density_by_Construct_stratified,
                                               SelfEfficacy_density_by_Construct_stratified,
                                               PhysicalFunctioning7_density_by_Construct_stratified)

density_ALL_Construct_quant_stratified = cbind(density_ALL_Construct_quant_stratified, height)



#plot4 = ggplot(density_ALL_Construct_quant_stratified, aes(x = Theta, y = construct, height=posterior, group = construct)) +
#  geom_density_ridges(stat = "identity", scale = 10) +
#  xlim(0, 0.5)
#plotting the results of the expert elicitation task 
Plot_Prior_qual_density = ggplot(density_ALL_Construct_quant_stratified, aes(x = logOddsRatio, y = Construct, height=Prior_qual_density, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-3, 4  )

print(Plot_Prior_qual_density)

#plotting likelihood (quantitative evidence only)
Plot_Likelihood_stratified = ggplot(density_ALL_Construct_quant_stratified, aes(x = logOddsRatio, y = Construct, height=Likelihood, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-3, 4  )
print(Plot_Likelihood_stratified)


#plotting the posterior resulted from updating prior with likelihood 
Plot_posterior_QualplusQuant = ggplot(density_ALL_Construct_quant_stratified, aes(x = logOddsRatio, y = Construct, height=posterior_QualplusQuant, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-3, 4  )  


print(Plot_posterior_QualplusQuant)



###########################################


###########################################

density_ALL_Construct_quant_stratified
head(density_ALL_Construct_quant_stratified)


All_constructs_prior = select(density_ALL_Construct_quant_stratified, logOddsRatio, Construct, Prior_qual_density) 

All_constructs_likelihood = select(density_ALL_Construct_quant_stratified, logOddsRatio, Construct, Likelihood)

All_constructs_posterior = select(density_ALL_Construct_quant_stratified, logOddsRatio, Construct, posterior_QualplusQuant) 


Age_density_prior = All_constructs_prior  %>% filter(Construct == 'Age')
SixMWT_density_prior = All_constructs_prior  %>% filter(Construct == "6MWT")
PhysicalFunctioning_density_prior = All_constructs_prior  %>% filter(Construct == "PhysicalFunctioning")
LVEF_density_prior = All_constructs_prior  %>% filter(Construct == "LVEF")
SelfEfficacy_density_prior = All_constructs_prior  %>% filter(Construct == "SelfEfficacy")



Age_density_likelihood = All_constructs_likelihood  %>% filter(Construct  == "Age")
SixMWT_density_likelihood = All_constructs_likelihood  %>% filter(Construct  == "6MWT")
PhysicalFunctioning_density_likelihood = All_constructs_likelihood  %>% filter(Construct  == "PhysicalFunctioning")
LVEF_density_likelihood = All_constructs_likelihood  %>% filter(Construct  == "LVEF")
SelfEfficacy_density_likelihood = All_constructs_likelihood  %>% filter(Construct  == "SelfEfficacy")


Age_density_posterior = All_constructs_posterior  %>% filter(Construct  == "Age")
SixMWT_density_posterior = All_constructs_posterior  %>% filter(Construct == "6MWT")
PhysicalFunctioning_density_posterior = All_constructs_posterior  %>% filter(Construct  == "PhysicalFunctioning")
LVEF_density_posterior = All_constructs_posterior  %>% filter(Construct == "LVEF")
SelfEfficacy_density_posterior = All_constructs_posterior  %>% filter(Construct  == "SelfEfficacy")


prior_name = rep("Qualitative evidence", times = 1000)

likelihood_name = rep("Quantitative evidence", times = 1000)

posterior_name = rep("Posterior (Qual + QUANT)", times = 1000)


distribution = c(prior_name, likelihood_name, posterior_name)


height = c(rep(10, 1000),
           rep(20, 1000), 
           rep(30, 1000), 
           rep(40, 1000), 
           rep(50, 1000), 
           rep(60, 1000), 
           rep(70, 1000), 
           rep(80, 1000), 
           rep(90, 1000),
           rep(100, 1000), 
           rep(110, 1000),
           rep(120, 1000), 
           rep(130, 1000), 
           rep(140, 1000), 
           rep(150, 1000))




d <- data.frame(
  logOddsRatio = density_ALL_Construct_quant_stratified$logOddsRatio, 
  Construct = c(Age_density_prior$Construct,
                Age_density_likelihood$Construct,
                Age_density_posterior$Construct,
                
               SixMWT_density_prior$Construct,
                SixMWT_density_likelihood$Construct,
                SixMWT_density_posterior$Construct,
                
                PhysicalFunctioning_density_prior$Construct,
                PhysicalFunctioning_density_likelihood$Construct,
                PhysicalFunctioning_density_posterior$Construct,
                
             
                LVEF_density_prior$Construct, 
                LVEF_density_likelihood$Construct,
                LVEF_density_posterior$Construct,
                
                SelfEfficacy_density_prior$Construct,
                SelfEfficacy_density_likelihood$Construct,
                SelfEfficacy_density_posterior$Construct),
  
  y = c(Age_density_prior$Prior_qual_density,
        Age_density_likelihood$Likelihood,
        Age_density_posterior$posterior_QualplusQuant,
        
    
        
        
        SixMWT_density_prior$Prior_qual_density,
        SixMWT_density_likelihood$Likelihood,
        SixMWT_density_posterior$posterior_QualplusQuant,
        
        PhysicalFunctioning_density_prior$Prior_qual_density,
        PhysicalFunctioning_density_likelihood$Likelihood,
        PhysicalFunctioning_density_posterior$posterior_QualplusQuant,
        
    
        
        LVEF_density_prior$Prior_qual_density, 
        LVEF_density_likelihood$Likelihood,
        LVEF_density_posterior$posterior_QualplusQuant,
        
        SelfEfficacy_density_prior$Prior_qual_density,
        SelfEfficacy_density_likelihood$Likelihood,
        SelfEfficacy_density_posterior$posterior_QualplusQuant),
  
  distribution = distribution, 
  
  height = height)

d$group_name = paste0(as.character(d$Construct)," ", as.character(d$distribution))
# colors for the intermediate plots were as follows: 
#prior: "#CC79A7"
#posterior: "#D55E00"
# likelihood: "#009E73"

#we want the distributions to be same color as they are in the intermediate plots so prior should be "#CC79A7" and so on. 
# however, they do not look nice together so I will pick the same colors but more pastel, Set2 is nice: 
#display.brewer.all(colorblindFriendly = TRUE)
#colors = display.brewer.pal(c(1, 2, 4), "Set2")
#print the codes for the colors: 
#brewer.pal(n = 5, name = "Set2")

##############

#colors from the set2: "#66C2A5" "#FC8D62" "#E78AC3"
Compare_distributions_plot_steps_per_day = ggplot(d, aes(x = logOddsRatio, 
                                           y = Construct,
                                           height = y, 
                                           group = group_name, 
                                           color = distribution,
                                           fill = distribution)) +
  
  geom_density_ridges(stat = "identity",
                      scale = 1) +
  
  #scale_fill_brewer(palette = "Set2")+
  #scale_color_brewer(palette = "Set2")+
  scale_fill_manual(values = c("#FC8D62" , "#E78AC3" ,"#66C2A5"))+
  scale_color_manual(values = c("#FC8D62" , "#E78AC3" ,"#66C2A5"))+
  
  xlim(-2,3) +
  
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.grid.major = element_line(colour = "grey", size = 0.2),
        panel.grid.minor = element_line(colour = "grey", size = 0.1)) 



print(Compare_distributions_plot_steps_per_day)



